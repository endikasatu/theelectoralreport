#' @title transform_polls
#'
#' @description Esta función prepara los datos descargados del agregador de encuestas de TheElectoralReport.
#'
#' @param data Dataframe con las encuestas.
#' @param .eday Día de la celebración de elecciones.
#'
#' @return Dataframe preparado con datos de las encuestas.
#'
#' @import tidyverse
#' @import dplyr
#' @import lubridate
#'
#' @export
#'

transform_polls <- function(data = "", .eday = ""){

  data %>%
    dplyr::select(-Tipo, -Provincia, -Municipio, -Fuente, -Lead, -Ganador) %>%
    separate(Encuestadora.Medio,
             into = c("Encuestadora", "Medio"),
             sep= "\\/",
             extra = "drop",
             fill = "right",
             remove = FALSE) %>%
    mutate(`Último.día` = ymd(`Último.día`),
           E.Day = ymd(.eday),
           Dias.Rest = as.numeric(E.Day - `Último.día`)) %>%
    dplyr::select(Literal,
                  CCAA,
                  Encuestadora.Medio,
                  Encuestadora,
                  Muestra,
                  Trabajo.Campo,
                  `Último.día`,
                  E.Day,
                  Dias.Rest,
                  `Participación`,
                  Ventaja,
                  everything()) %>%
    dplyr::select(-Medio)

  #rename_at(as.vector(na.omit(names$literal[match(names(data), names$literal)])),
  #          ~as.vector(na.omit(names$id_party[match(names(data), names$literal)])))

  }
