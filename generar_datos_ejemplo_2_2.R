#' @title Genera datos de ejemplo de textos para ACUACAR
#' 
#' @description Utilizando como base los datos con los que se entreno el modelo,
#' toma una muestra de datos que sirven como ejemplo de un nuevo conjunto de 
#' datos con el cual se van a generar predicciones. Asume que los datos se
#' encuentran aquí: 
#' metadatos: "datos/PQR ABOX 2021-03-26-V3-FUNDIDO.xlsx"
#' texto: "datos/as400.feather"
#' 
#' @param numero es el número de muestras a generar
#'

args <- commandArgs(trailingOnly = TRUE)

numero <- as.numeric(args[1])

library(readxl)
library(readr)
library(dplyr)
library(openxlsx)


# datos <- arrow::read_feather(text_file) %>%
#   select(name,text) %>% 
#   slice_sample(n = numero) 

datos <- read_excel("datos/PQR ABOX 2021-03-26-V3-FUNDIDO.xlsx",
                    sheet = "PQRS TOTAL") %>%
  slice_sample(n = numero) 


# datos %>%
#   write_csv("muestra_2_2.xlsx",overwrite = TRUE, sheetName = "PQRS TOTAL")

datos %>%
  write_csv("muestra_2_2.csv")

