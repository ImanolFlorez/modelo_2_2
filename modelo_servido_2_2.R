args <- commandArgs(trailingOnly = TRUE)

#' Predice con el modelo de solucion PQR con textos de Acuacar (modelo 2.2)
#'
#' Asume que:
#'  - El modelo fue creado previamente y esta almacenado en un archivo: "modelo_2_2.rds"
#'  - 
#'
#' @param datos_file nombre del archivo .csv con los datos a predecir (metadatos)
#' @param text_file nombre del archivo .feather donde se encuentran los textos
#' @param resultados nombre del archivo .csv con los resultados de las predicciones
#'
#' @return Escribe archivo con los resultados de las predicciones
#' 
#' @description En los parametros de entrada, nombre puede indicar también el 
#' directorio a la carpeta en cuestión.
#' `text_file` es donde se encuentran todos los textos.
#'  
predecir_textos <- function(datos_file,text_file,resultados_file) {
  library(readr)
  library(janitor)
  library(tidymodels)
  library(textrecipes)
  library(readxl)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(qdapRegex)

  source("R/ingestion_nuevos.R")
  
  
  #datos_file ="muestra_texto_favorabilidad_pqrs.csv"
  datos_file <- args[1]
   #text_file = "muestra_texto_favorabilidad_pqrs.csv"
  text_file <-  args[2]
  lexicon_file = "datos/lexicon_español.rds"
  vacias_file = "datos/vacias.txt"
  procesados_file = "datos/procesados.rds" 

  ingerir_nuevos_textos(datos_file,text_file,
                 lexicon_file = "datos/lexicon_español.rds",
                 vacias_file = "datos/vacias.txt",
                 procesados_file = "procesados.rds") 
  
  muestra_textos <- readRDS(here("datos","procesados.rds")) %>% 
    mutate(tipo_solucion = as.character(tipo_solucion))
  
  
  ### Obtener modelo -------------------------------------------------------------
  modelo <- readRDS("modelo_2_2.rds")

  ### Obtener predicciones -------------------------------------------------------
  clase <- predict(modelo,new_data = muestra_textos, type = "class")
  probabilidad <- predict(modelo,new_data = muestra_textos, type = "prob")

  ### Calcular y escribir resultados ---------------------------------------------
  resultados <- cbind.data.frame(clase,probabilidad)
  write_csv(resultados,file = resultados_file)

  }

### LLamar a la funcion con el nombre de los parametros dados ------------------
entrada_meta <- args[1]
entrada_text <- args[2]
salida <- args[3]

predecir_textos(entrada_meta,entrada_text,salida)
