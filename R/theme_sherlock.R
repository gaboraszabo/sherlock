#' Theme Sherlock
#'
#' @description
#' Set Sherlock plot theme
#'
#' @return Plot object with Sherlock plot theme
#'
#' @export

theme_sherlock <- function() {

  ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(color = "grey50", face = "bold", size = 11),
      #panel.border     = ggplot2::element_blank(),
      panel.spacing    = ggplot2::unit(1, "lines"),
      axis.ticks       = ggplot2::element_blank(),
      axis.title.x     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.title.y     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.text        = ggplot2::element_text(size = 11, color = "grey50"),
      legend.title     = ggplot2::element_text(color = "grey50", size = 11),
      legend.text      = ggplot2::element_text(color = "grey50", size = 11),
      plot.title       = ggplot2::element_text(color = "grey50", size = 14, hjust = 0),
      plot.subtitle    = ggplot2::element_text(color = "grey50", size = 11, hjust = 0),
      plot.caption     = ggplot2::element_text(color = "grey50", size = 8))

}
