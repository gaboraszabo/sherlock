#' Draw Polar Small Multiples
#'
#' @description
#' Draws a Polar Small Multiples Plot
#'
#' @param data input dataset to be plotted (required)
#' @param angular_axis angular coordinate values (required)
#' @param x_y_coord_axis x-y coordinate values (required)
#' @param grouping_var grouping variable (required)
#' @param faceting_var_1 Set first faceting variable (optional)
#' @param faceting_var_2 Set second faceting variable (optional)
#' @param connect_with_lines Logical. If set to TRUE, values within each group are connected with a line. By default, it is set to FALSE (optional)
#' @param connect_start_and_end_points Logical. If set to TRUE, the start and end points of the lines get connected. It is useful when trying to draw a complete circle but may not be useful when only trying to draw a shape different than that (e.g. a semicircle). By default, it is set to TRUE (optional)
#' @param x_y_coord_axis_limits Set x-y coordinate axis limits. By default, it is set to start at 0. (optional)
#' @param point_size Set point size. By default, it is set to 2  (optional)
#' @param line_size Set line size. By default, it is set to 0.6  (optional)
#' @param point_alpha Set point transparency. By default, it is set to 0.6  (optional)
#' @param line_alpha Set line transparency. By default, it is set to 0.5  (optional)
#' @param label_text_size Size of text for labels. By default, it is set to 11. (optional)
#' @param analysis_desc_label Label (subtitle) for analysis description. By default, it is set to NULL. (optional)
#'
#' @return A 'ggplot' object
#'
#' @examples
#' library(dplyr)
#'
#' polar_small_multiples_data %>%
#'   filter(Mold_Cavity_Number %in% c(4, 6)) %>%
#'   draw_polar_small_multiples(angular_axis   = ID_Measurement_Angle,
#'                              x_y_coord_axis = ID_2,
#'                              grouping_var   = Tip_Bottom,
#'                              faceting_var_1 = Mold_Cavity_Number,
#'                              point_size     = 0.5,
#'                              connect_with_lines = TRUE,
#'                              label_text_size = 7)
#'
#' @export
#'

draw_polar_small_multiples <- function(data, angular_axis, x_y_coord_axis,
                                       grouping_var, faceting_var_1, faceting_var_2,
                                       connect_with_lines = FALSE, connect_start_and_end_points = TRUE, x_y_coord_axis_limits = c(0, NA),
                                       point_size = 2, line_size = 0.6, point_alpha = 0.6, line_alpha = 0.5,
                                       label_text_size = 11, analysis_desc_label = "") {

  x_expr            <- rlang::enquo(angular_axis)
  y_expr            <- rlang::enquo(x_y_coord_axis)
  grouping_var_expr <- rlang::enquo(grouping_var)
  faceting_var_1_expr <- rlang::enquo(faceting_var_1)
  faceting_var_2_expr <- rlang::enquo(faceting_var_2)


  plot <- data %>%
    dplyr::mutate(!! grouping_var_expr := forcats::as_factor(!! grouping_var_expr)) %>%

    ggplot2::ggplot(ggplot2::aes(!!x_expr, !!y_expr, color = !!grouping_var_expr)) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::scale_x_continuous(breaks = c(0, 90, 180, 270),
                                limits = c(0, 360),
                                labels = scales::number_format(suffix = "\u00b0")) +
    ggplot2::scale_y_continuous(limits = x_y_coord_axis_limits) +
    ggplot2::theme_light() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(color = "grey70", face = "bold", size = 11),
      panel.border     = ggplot2::element_blank(),
      panel.spacing    = ggplot2::unit(1, "lines"),
      axis.title.x     = ggplot2::element_text(size = label_text_size, color = "grey50"),
      axis.title.y     = ggplot2::element_text(size = label_text_size, color = "grey50"),
      axis.text        = ggplot2::element_text(size = label_text_size, color = "grey50"),
      legend.title     = ggplot2::element_text(color = "grey50", size = label_text_size),
      legend.text      = ggplot2::element_text(color = "grey50", size = label_text_size),
      plot.title       = ggplot2::element_text(color = "grey50", size = 16, hjust = 0),
      plot.subtitle    = ggplot2::element_text(color = "grey50", size = 11, hjust = 0),
      plot.caption     = ggplot2::element_text(color = "grey50", size = 8)) +
    ggplot2::labs(
      title    = "Polar Small Multiples Plot",
      subtitle = analysis_desc_label
    ) +
    sherlock::scale_color_sherlock()

  if (connect_with_lines) {
    if (connect_start_and_end_points) {
      plot <- plot + ggplot2::geom_path(size = line_size, alpha = line_alpha)
    }

    if (!connect_start_and_end_points) {
      plot <- plot + ggplot2::geom_line(size = line_size, alpha = line_alpha)
    }

  }

  if (!missing(faceting_var_1) & missing(faceting_var_2)) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!faceting_var_1_expr))
  }


  if (!missing(faceting_var_1) & !missing(faceting_var_2)) {
    plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(!!faceting_var_1_expr),
                                       cols = ggplot2::vars(!!faceting_var_2_expr))
  }

  return(plot)

}
