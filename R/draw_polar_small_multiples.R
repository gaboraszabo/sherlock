#' Draw Polar Small Multiples
#'
#' @description
#' Draws a Polar Small Multiple Plot
#'
#' @param data input dataset to be plotted (required)
#' @param x x coordinate values (required)
#' @param y y coordinate values (required)
#' @param grouping_var grouping variable (required)
#' @param connect_with_lines logical. if FALSE, default, values within each group are not connected with a line (optional)
#'
#' @return A ggplot polar small multiples object
#' @export
#'

draw_polar_small_multiples <- function(data, x, y, grouping_var, connect_with_lines = FALSE) {

  x_expr            <- rlang::enquo(x)
  y_expr            <- rlang::enquo(y)
  grouping_var_expr <- rlang::enquo(grouping_var)


  plot <- data %>%
    dplyr::mutate(!! grouping_var_expr := forcats::as_factor(!! grouping_var_expr)) %>%

    ggplot2::ggplot(ggplot2::aes(!!x_expr, !!y_expr, color = !!grouping_var_expr)) +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::scale_x_continuous(breaks = c(0, 90, 180, 270),
                                limits = c(0, 360),
                                labels = scales::number_format(suffix = "\u00b0")) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_light() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(color = "grey70", face = "bold", size = 11),
      panel.border     = ggplot2::element_blank(),
      panel.spacing    = ggplot2::unit(1, "lines"),
      axis.title.x     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.title.y     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.text        = ggplot2::element_text(size = 11, color = "grey50"),
      legend.title     = ggplot2::element_text(color = "grey50", size = 11),
      legend.text      = ggplot2::element_text(color = "grey50", size = 11),
      plot.title       = ggplot2::element_text(color = "grey50", size = 14, hjust = 0),
      plot.subtitle    = ggplot2::element_text(color = "grey50", size = 11, hjust = 0),
      plot.caption     = ggplot2::element_text(color = "grey50", size = 8)) +
    ggplot2::scale_color_manual(values = c("#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A"))

  if (connect_with_lines) {
    plot <- plot + ggplot2::geom_line(alpha = 0.4)
  }

  return(plot)

}
