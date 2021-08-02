#' @title save_plot
#'
#' @description Esta función exporta los gráficos a un directorio.
#'
#' @import ggplot2
#'
#' @export

 save_plot <- function (plot, width = 575, height = 500, save_path, plot_name, extension = ".pdf") {

   ggplot2::ggsave(filename = paste0(save_path, plot_name, extension),
                   plot = plot,
                   width = (width/96),
                   height = (height/96))
 }



