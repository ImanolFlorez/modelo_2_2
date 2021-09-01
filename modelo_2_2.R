#' @title Entrena el modelo de solución de PQRS para Acuacar y genera el binario,
#' la ficha con las medidas de rendimiento del modelo y la matriz de confusión 
#' 
#' @description ESte modelo es el codificado como 2.2. Se presume que con antelación
#' se ejecutó el script de ingestión de datos y en la carpeta datos se encuentra
#' el archivo todo_textos.rds resultante y la lista de palabras vacías (vacias.txt) 
#' 
#' 

## ----setup, include=FALSE--------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(tidymodels)
library(tidytext)
library(stopwords)
library(textrecipes)
library(themis)
library(tictoc)
library(here)
library(ranger)
library(doParallel)
library(forcats)
library(fs)

all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores - 2)

otras_vacias <- read_lines(here("datos","vacias.txt"))
vacias <- c(stopwords("es"), otras_vacias)
set.seed(4321)

n_tokens <-150
quiebre <- 1
n_trees <- 500

semilla <- 9835
set.seed(semilla)

source("R/ingestion.R")

## --------------------------------------------------------------------------------------

if (!file_exists("datos/todo_textos.rds")) {
  print("Ingestión de textos..")
  ingerir_textos("PQR ABOX 2021-03-26-V3-FUNDIDO.xlsx","as400.feather",
                 "lexicon_español.rds","vacias.txt","todo_textos.rds")
}

todo_textos <- readRDS(here("datos","todo_textos.rds")) %>% 
  mutate(tipo_solucion = as.character(tipo_solucion))


## --------------------------------------------------------------------------------------
favorable <- c("F","1","2","5","6")
desfavorable <- c("D","3","4","7")
otros <- c("8","9","10","11")


 
todo_textos <- todo_textos %>%
  mutate(
    tipo_solucion = case_when(
      tipo_solucion %in% favorable ~ "favorable",
      tipo_solucion %in% desfavorable ~ "desfavorable",
      tipo_solucion %in% otros  ~ "otros",
      TRUE ~ tipo_solucion)
      ) %>%
  drop_na(tipo_solucion) %>% 
  filter(tipo_solucion != "otros") %>% 
  mutate(tipo_solucion = fct_drop(tipo_solucion)) 


## ----division--------------------------------------------------------------------------
division <- initial_split(todo_textos, strata = tipo_solucion)
entrenamiento <- training(division)
prueba <- testing(division)


## ----especificacion modelo-------------------------------------------------------------
rf_spec <- rand_forest() %>%
  set_args(trees = n_trees) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")


## ----receta modelo---------------------------------------------------------------------
receta <- recipe(tipo_solucion ~ text + 
                   PC1 + PC2,
                 data = entrenamiento) %>% 
  step_upsample(tipo_solucion, over_ratio = 1) %>% 
  step_tokenize(text) %>% 
  step_stopwords(text,
                 custom_stopword_source = vacias,
                 language = "es") %>%
  step_tokenfilter(text,max_tokens = n_tokens) %>% 
  step_tfidf(text) %>% 
  step_corr(all_numeric())


## ----flujo modelo----------------------------------------------------------------------
flujo <- workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(receta)


## ----ajustar modelo--------------------------------------------------------------------

tic()
 modelo_rf <- flujo %>% 
   last_fit(split = division)
toc()



## ----evaluar modelo--------------------------------------------------------------------
metricas <- collect_metrics(modelo_rf)
predicciones <- collect_predictions(modelo_rf)


## ----metricas prueba-------------------------------------------------------------------

correlacion_matthews <- mcc(predicciones,truth = tipo_solucion, estimate = .pred_class)

final_res_metrics <- bind_rows(metricas, correlacion_matthews) %>% 
  select(-.config)

final_res_metrics


## ----confusion final-------------------------------------------------------------------

matriz_confusion <- predicciones %>%
  conf_mat(truth = tipo_solucion, estimate = .pred_class) 

write.table(matriz_confusion$table,"matriz_confusion_mod_2_2.csv", sep = ",")

write_csv(final_res_metrics,"ficha_mod_2_2.csv")



## ---- eval = FALSE, guardar------------------------------------------------------------

modelo <- modelo_rf$.workflow[[1]]
saveRDS(modelo, file = "modelo_2_2.rds")

