#'Un par de funciones de utilidad para hacer y visualizar tablas de frecuencias
#'de acuerdo a una categoría dada

#'Hace una tabla de frecuencias de un conjunto de datos acuerdo al criterio dado 
#'
#' @title Hacer una tabla de frecuencias ordenada de mayor a menor
#' @description Hace una tabla de frecuencias de un conjunto de datos *df* de 
#' contando el número de observaciones de acuerdo a *criterio*
#'
#' @return una tabla de frecuencias ordenada

hacer_tabla <- function(df,criterio) {
  df %>%
    group_by({{criterio}}) %>%
    summarize(frecuencia = n()) %>%
    arrange(desc(frecuencia)) %>%
    mutate(
      total = sum(frecuencia),
      porcentaje = (frecuencia/total)*100,
      acumulado = cumsum(porcentaje),
      posicion = 1:n()
    ) %>%
    select(-total) %>%
    ungroup()
}

#'Mejora la presentación de las tablas de frecuencia 
#'
#' @title Presenta una tabla de frecuencias
#' @description Mejora la presentación de una tabla de frecuencias
#'
#' @return la visualización de la tabla

presentar_tabla <- function(df) {
    df %>%
    gt() %>%
    fmt_number(
      columns = 2,
      decimals = 0,
      use_seps = TRUE
    ) %>%
    fmt_number(
      columns = 3:4,
      decimals = 1,
      use_seps = FALSE
    ) %>%
    cols_label(
      frecuencia = "Frecuencia",
      porcentaje = "%",
      acumulado = "Acumulado",
      posicion = " ")
}

graficar_tabla <- function(df,criterio,cuantos = 10) {
  df %>%
    filter(posicion <= cuantos) %>%
    ggplot(aes(x = reorder({{criterio}},frecuencia), y = frecuencia)) +
    geom_col(fill = "steel blue") +
    xlab(NULL) +
    ylab("Frecuencia") +
    scale_x_discrete(labels = function(x)
      str_wrap(str_replace_all(x,"/"," "),
               width = 20)) +
    coord_flip()
}
