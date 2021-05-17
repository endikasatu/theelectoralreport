#' @title download_polls
#'
#' @description Esta función descarga las encuestas recopiladas por TheElectoralReport en función de los filtros escogidos y los guarda en un directorio especificado.
#'
#' @param tipo El tipo de elección que se quiere descargar. Pueden ser de tres tipos: "generales", "autonómicas" o "municipales".
#' @param ccaa La Comunidad Autónoma donde se realiza la encuesta.
#' @param prov La Provincia donde se realiza la encuesta.
#' @param mun El municipio donde se realiza la encuesta.
#' @param pollster Encuestadora y medio de comunicación que publica la encuesta.
#' @param dir La ruta a la carpeta donde se quiere guardar la descarga.
#'
#' @examples df <- download_polls(tipo = "generales", pollster = "CIS", dir = "tu_dirección")
#'
#' @return Dataframe con los datos de las encuestas especificadas.
#'
#' @import dplyr
#' @import janitor
#' @import stringr
#'
#' @export

download_polls <- function(tipo = "", ccaa = "", prov = "", mun = "", pollster = "", dir) {

   # Leer encuestas desde la base de datos de TheElectoralReport

   encuestas <- read.csv("https://raw.githubusercontent.com/endikasatu/encuestas_esp/main/data/spain_latest_polls.csv",
                         stringsAsFactors = FALSE, encoding = "UTF-8")

   encuestas[,c("Tipo","CCAA", "Provincia", "Municipio", "Encuestadora.Medio")][encuestas[,c("Tipo","CCAA", "Provincia", "Municipio", "Encuestadora.Medio")] == ""] <- "-"
   encuestas[,c("Tipo","CCAA", "Provincia", "Municipio", "Encuestadora.Medio")][is.na(encuestas[,c("Tipo","CCAA", "Provincia", "Municipio", "Encuestadora.Medio")])] <- "-"

   # Crear listas para filtrar

   lista_tipo <- paste(encuestas$Tipo, collapse = " ")
   lista_ccaa <- paste(encuestas$Literal, collapse = " ")
   lista_prov <- paste(encuestas$Provincia, collapse = " ")
   lista_mun <- paste(encuestas$Municipio, collapse = " ")
   lista_pollster <- paste(encuestas$Encuestadora.Medio, collapse = " ")

   # Comprobar inputs convirtiendo en mayus, minus o capital
   ## Tipo

   if (tipo %in% lista_tipo == TRUE | tipo == "") {
     tipo <- tipo
   } else if (stringr::str_detect(lista_tipo, stringr::str_to_title(tipo)) == TRUE | tipo == "") {

     tipo <- stringr::str_to_title(tipo)
   } else if (stringr::str_detect(lista_tipo, tolower(tipo)) == TRUE  | tipo == "") {
     tipo <- tolower(tipo)
   } else if (stringr::str_detect(lista_tipo, toupper(tipo)) == TRUE | tipo == "") {
     tipo <- toupper(tipo)
   } else {
     tipo<- ""
     cat("ATENCIÓN!!!!! No hay coincidencias de la Encuestadora indicada")
   }

   ## Municipio

   if (mun %in% lista_prov == TRUE | mun == "") {
     mun <- mun
   } else if (stringr::str_detect(lista_mun, stringr::str_to_title(mun)) == TRUE | mun == "") {

     mun <- stringr::str_to_title(mun)
   } else if (stringr::str_detect(lista_mun, tolower(mun)) == TRUE | mun == "") {
     mun <- tolower(mun)
   } else if (stringr::str_detect(lista_mun, toupper(mun)) == TRUE | mun == "") {
     mun <- toupper(mun)
   } else {
     mun<- ""
     cat("ATENCIÓN!!!!! No hay coincidencias de la Encuestadora indicada")
   }

   ## Provincia

   if (prov %in% lista_prov == TRUE | prov == "") {
     prov <- prov
   } else if (stringr::str_detect(lista_prov, stringr::str_to_title(prov)) == TRUE | prov == "") {

     prov <- stringr::str_to_title(prov)
   } else if (stringr::str_detect(lista_prov, tolower(prov)) == TRUE  | prov == "") {
     prov <- tolower(prov)
   } else if (stringr::str_detect(lista_prov, toupper(prov)) == TRUE | prov == "") {
     prov <- toupper(prov)
   } else {
     prov<- ""
     cat("ATENCIÓN!!!!! No hay coincidencias de la Encuestadora indicada")
   }

   ## CCAA

   if (ccaa %in% lista_ccaa == TRUE | ccaa == "") {
     ccaa <- ccaa
   } else if (stringr::str_detect(lista_ccaa, stringr::str_to_title(ccaa)) == TRUE | ccaa == "") {
     ccaa <- stringr::str_to_title(ccaa)
   } else if (stringr::str_detect(lista_ccaa, tolower(ccaa)) == TRUE  | ccaa == "") {
     ccaa <- tolower(ccaa)
   } else if (stringr::str_detect(lista_ccaa, toupper(ccaa)) == TRUE | ccaa == "") {
     ccaa <- toupper(ccaa)
   } else {

     ccaa<- ""
     cat("ATENCIÓN!!!!! No hay coincidencias de la CCAA. Puede probar con:")
     cat(unique(encuestas$Literal))
   }

   ## Encuestadora/Medio

   if (pollster %in% lista_pollster == TRUE | pollster == "") {
      pollster <- pollster
   } else if (stringr::str_detect(lista_pollster, stringr::str_to_title(pollster)) == TRUE | pollster == "") {

     pollster <- stringr::str_to_title(pollster)
   } else if (stringr::str_detect(lista_pollster, tolower(pollster)) == TRUE  | pollster == "") {
     pollster <- tolower(pollster)
   } else if (stringr::str_detect(lista_pollster, toupper(pollster)) == TRUE | pollster == "") {
     pollster <- toupper(pollster)
   } else {
     pollster<- ""
     cat("ATENCIÓN!!!!! No hay coincidencias de la Encuestadora indicada")
     }

   # Crear dataframe con los filtros asignados

   df_encuestas<- encuestas %>%
   filter(if(tipo != "") stringr::str_detect(Tipo, tipo) else Tipo != "",
          if(ccaa != "") stringr::str_detect(Literal, ccaa) else CCAA != "",
          if(prov != "") stringr::str_detect(Provincia, prov) else Provincia != "",
          if(mun != "") stringr::str_detect(Municipio, mun) else Municipio != "",
          if(pollster != "") stringr::str_detect(Encuestadora.Medio, pollster) else Encuestadora.Medio != "") %>%
     janitor::remove_empty(which = "cols") %>%
     dplyr::select(-X)

   # Comprobar si los filtros asignados devuelven dataframe vacío

   if (nrow(df_encuestas) < 1) {

     cat("ATENCIÓN!!!!! No se han encontrado coincidencias")
     df_encuestas<- NULL

   } else {

     # Establecer ruta de descarga y crear .csv

     path <- paste0(dir, "/", "df_polls.csv")
     write.csv(df_encuestas, path)

     return(df_encuestas)

     }
}
