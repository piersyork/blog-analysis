#' @export
theme_piers <- function() {
  box::use(ggplot2[...])
  thm <- theme_minimal(base_family = "serif") %+replace%
    theme(text = element_text(colour = "#373737", size = 14),
          plot.title = element_text(size = "20", hjust = 0, vjust = 3.2),
          plot.subtitle = element_text(hjust = 0, vjust = 3.5),
          plot.title.position = "plot",
          legend.position = "right",
          axis.text = element_text(face = "plain"),
          panel.grid.major = element_line(linetype = "dashed"),
          panel.grid.minor = element_blank(),
          plot.margin = margin(11, 15, 5, 10),
          axis.line.x = element_line(colour = "#8e8e8e"),
          axis.ticks = element_line(colour = "#8e8e8e"))

  return(thm)
}
