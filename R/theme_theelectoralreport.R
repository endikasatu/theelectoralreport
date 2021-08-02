#' @title theme_theelectoralreport
#'
#' @description Esta función predefine un tema o estilo de TheElectoralReport para los gráficos.
#'
#' @import ggplot2
#'
#' @export

theme_theelectoralreport<- function() {
  ggplot2::theme(
    plot.title = ggplot2::element_text(family = "Helvetica",
                                       size = 18,
                                       face = "bold",
                                       color = "#222222"),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(family = "Helvetica",
                                          size =  14,
                                          color = "#222222"),
    plot.caption = ggplot2::element_text(family = "Helvetica",
                                         size = 10,
                                         color = "#7A7A7A",
                                         hjust = c(0,1)),
    plot.caption.position = "plot",
    axis.title = ggplot2::element_text(family = "Helvetica",
                                       size = 12,
                                       face = "bold",
                                       color = "#222222",
                                       ),
    axis.text = ggplot2::element_text(family = "Helvetica",
                                      size = 12,
                                      color = "#7A7A7A"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.text.y = ggplot2::element_text(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #legend.position = c(.95, .95),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(1, 1, 1, 1),
    
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = "Helvetica",
                                        size= 10,
                                        color= "#7A7A7A"),
    legend.box = "vertical",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#CCCCCC"),
    panel.grid.major.x = ggplot2::element_line(color="#CCCCCC"),
    
    panel.background = ggplot2::element_rect(fill = "#F5F5F5",
                                             colour = "#F5F5F5",
                                             size = 0.5,
                                             linetype = "solid"),
    strip.background = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(size  = 13,
                                       face = "bold",
                                       color = "#222222",
                                       hjust = 0),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
    )
}
      
    
    

            