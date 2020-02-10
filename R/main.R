library(tidyverse)
#library(magrittr)
library(base64enc)
library(tuneR)

ler_audio <- function(arq_aud) {
  tuneR::readWave(arq_aud)@left
}

quebrar_letras <- function(onda) {
  d_clean <- tibble::tibble(
    som = onda,
    tempo = 1:length(onda)
  ) %>%
    dplyr::mutate(
      som = ifelse(abs(som) < 2000, 0, som),
      abs_som = abs(som),
      acu = cumsum(abs_som)
    ) %>%
    dplyr::arrange(dplyr::desc(tempo)) %>%
    dplyr::mutate(revacu = cumsum(abs_som)) %>%
    dplyr::arrange(tempo) %>%
    dplyr::filter(acu > 0, revacu > 0)

  cortes <- d_clean %>%
    dplyr::filter(som == 0) %>%
    dplyr::mutate(lg = dplyr::lag(tempo), trocou = (tempo != lg + 1)) %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(n = cumsum(trocou) + 1) %>%
    dplyr::group_by(n) %>%
    dplyr::summarise(xi = min(tempo), xf = max(tempo), r = xf - xi) %>%
    dplyr::arrange(dplyr::desc(r)) %>%
    head(3) %>%
    dplyr::mutate(corte = round((xf + xi) / 2)) %>%
    with(corte) %>%
    sort()

  d_clean %>%
    dplyr::mutate(
      letra = cut(tempo, c(0, cortes, max(tempo))),
      letra = as.character(as.numeric(letra))
    )
}

tabelar_letras <- function(arq_aud) {
  onda <- ler_audio(arq_aud)
  # desenhar_corte(onda)
  onda %>%
    quebrar_letras() %>%
    dplyr::group_by(letra) %>%
    dplyr::summarise(soma = sum(som))
}

mais_perto <- function(d, soma) {
  d$resp[which.min((d$soma - soma) ^ 2)][1]
}

predizer <- function(arq_aud) {
  d <- tibble::tribble(
    ~resp, ~ soma,
    "0", 934147,
    "2", 1290859,
    "3", 74808,
    "4", 335953,
    "5", 134279,
    "6", 197053,
    "7", 226359,
    "8", 427192,
    "9", 1668529,
    "a", 124376,
    "b", 6172811,
    "c", 16365602,
    "d", 12093844,
    "e", 15738223,
    "f", 15953836,
    "g", 17201763,
    "h", 16661981,
    "i", 12958768,
    "j", 13986420,
    "k", 10440971,
    "m", 17891685,
    "n", 17292889,
    "o", 11157747,
    "p", 9786082,
    "q", 8576722,
    "r", 13521870,
    "s", 13341856,
    "t", 10371958,
    "u", 10544620,
    "v", 13161335,
    "w", 13788643,
    "x", 17166469,
    "y", 16652035,
    "z", 13875621
  )
  arq_aud %>%
    tabelar_letras() %>%
    dplyr::mutate(soma = abs(soma)) %>%
    dplyr::mutate(res = purrr::map_chr(soma, ~ mais_perto(d, .x))) %>%
    with(res) %>%
    glue::glue_collapse()
}

#' @title Break captchas sound
#'
#' @description Given one mp3 and one guid code.
#'
#' @param mp3_encoded .
#' @param guid
#'
#' @export
decryptSound <- function(mp3_encoded, guid) {
  mp3 <- stringr::str_c(guid, ".mp3")
  wav <- stringr::str_c(guid, ".wav")

  #Salva o mp3 localmente
  fs <- file(mp3, "wb")
  base64enc::base64decode(mp3_encoded, fs)
  close(fs)

  #lê o mp3 para conversão Wav
  rmp3 <- tuneR::readMP3(mp3)

  #converte o mp3 para wav
  tuneR::writeWave(rmp3, wav, extensible = FALSE)

  p <- predizer(wav)

  file.remove(mp3)
  file.remove(wav)

  return(p)
}


