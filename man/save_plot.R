#' @title theme_theelectoralreport
#'
#' @description Esta funciÃ³n predefine un tema o estilo de TheElectoralReport para los grÃ¡ficos.
#'
#' @import ggplot2
#'
#' @export
#' 

save_plot <- function (width = 575, height = 500, save_path, plot_name, extension = ".pdf") {
  
  ggplot2::ggsave(filename = paste0(save_path, plot_name, extension),
                  width = (width/96),
                  height = (height/96))
}
