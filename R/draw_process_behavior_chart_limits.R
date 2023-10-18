#' Draw Process Behavior Chart Limits
#'
#' @description
#' Draws limits (LCL, UCL) and grand average for a Process Behavior Chart
#'
#' @param data Input dataset to be plotted (required)
#' @param x_var X variable to be plotted on X axis (required)
#' @param avg_line_color Set the color of the grand average line. By default, it is set to "grey60".
#' @param avg_line_alpha Set the transparency of the grand average line. By default, it is set to 0.8.
#' @param avg_line_width Set the line width of the grand average line. y default, it is set to 1.
#' @param limits_color Set the color of the natural process limits (control limits). By default, it is set to "red".
#' @param limits_alpha Set the transparency of the natural process limits (control limits). By default, it is set to 0.5.
#' @param limits_width Set the line width of the natural process limits (control limits). y default, it is set to 1.
#'
#' @return A ggplot object
#'
#' @export

draw_process_behavior_chart_limits <- function(data, x_var,
                                               avg_line_color = "grey60", avg_line_alpha = 0.8, avg_line_width = 1,
                                               limits_color = "red", limits_alpha = 0.5, limits_width = 1) {

  x_var_expr        <- rlang::enquo(x_var)

  geom <- data %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(!!x_var_expr, Average), color = avg_line_color, alpha = avg_line_alpha, linetype = "dashed", linewidth = avg_line_width) +
    ggplot2::geom_line(ggplot2::aes(!!x_var_expr, LCL), color = limits_color, alpha = limits_alpha, linewidth = limits_width) +
    ggplot2::geom_line(ggplot2::aes(!!x_var_expr, UCL), color = limits_color, alpha = limits_alpha, linewidth = limits_width)

  return(geom)

}
