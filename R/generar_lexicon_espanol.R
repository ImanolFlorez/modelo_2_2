#'Lee el lexicon de sentimientos de NRC en español (la versión no comercial) 
#'(https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm)
#'
#' @title Lee el lexicon de sentimientos de NRC
#' @description Lee le lexicon en español. Convierte las palabras a minúsculas y 
#' sin acentos y lo guarda como un objeto .rds en el directorio de datos 
#' correspondiente. Asume que el archivo: 
#' "NRC-Emotion-Lexicon-v0.92-In105Languages-Nov2017Translations.xlsx" 
#' fue descargado de la página y colocado en la carpeta datos del directorio
#' de trabajo
#'
#' @return "lexicon_español.rds

library(readxl)
library(dplyr)
library(here)
library(janitor)
library(readr)

# Lee el archivo
lexicon_sentimientos <- read_excel(here("datos",
  "NRC-Emotion-Lexicon-v0.92-In105Languages-Nov2017Translations.xlsx")) %>%
  clean_names()

# Toma las columnas en inglés y español junto con los sentimientos asociados
lexicon_sentimientos <- lexicon_sentimientos %>%
  select(english_en_1,
         spanish_es,
         positive,
         negative,
         anger,
         anticipation,
         disgust,
         fear,
         joy,
         sadness,
         surprise,
         trust) %>%
  rename(word = spanish_es) %>%     # para facilitar joins mas adelante
  mutate(
    # quita acentos a todas las palabras
    word = iconv(word,from = "UTF-8",to = "ASCII//TRANSLIT"),
    word = str_to_lower(word)
  )



# Guarda para que se usado en los programas
saveRDS(lexicon_sentimientos,here("datos","lexicon_español.rds"))
