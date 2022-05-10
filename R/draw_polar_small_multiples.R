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
    ggplot2::geom_point() +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::scale_x_continuous(breaks = c(0, 90, 180, 270),
                                limits = c(0, 360)) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_light()

  if (connect_with_lines) {
    plot <- plot + ggplot2::geom_line()
  }

  return(plot)

}
