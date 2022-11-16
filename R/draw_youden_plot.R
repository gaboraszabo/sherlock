#' Draw Youden Plot
#'
#' @description
#' Draws a Youden Plot
#'
#' @param data input dataset to be plotted (required)
#' @param x_axis_var variable to be plotted on x axis (required)
#' @param y_axis_var variable to be plotted on x axis (required)
#' @param grouping_var grouping variable (optional)
#' @param lsl lower specification limit (optional)
#' @param usl upper specification limit (optional)
#' @param median_line logical. If TRUE, a median bias line is plotted. By default, it is set to FALSE  (optional)
#' @param size Set point size. By default, it is set to 2  (optional)
#' @param alpha Set transparency. By default, it is set to 0.4  (optional)
#' @param analysis_desc_label Label (subtitle) for analysis description. By default, it is set to NULL  (optional)
#' @param x_axis_label Label for x axis. By default, it is set to display x axis column name  (optional)
#' @param y_axis_label Label for y axis. By default, it is set to display y axis column name  (optional)
#'
#' @return A 'ggplot' object
#'
#' @examples
#' youden_plot_data %>%
#'     draw_youden_plot(x_axis_var   = measurement_1,
#'                      y_axis_var   = measurement_2,
#'                      grouping_var = location)
#'
#' youden_plot_data_2 %>%
#'     draw_youden_plot(x_axis_var  = gage_1,
#'                      y_axis_var  = gage_2,
#'                      median_line = TRUE)
#'
#' @export

draw_youden_plot <- function(data, x_axis_var, y_axis_var, grouping_var, lsl, usl,
                             median_line = FALSE, size = 2, alpha = 0.4, analysis_desc_label = NULL,
                             x_axis_label = NULL, y_axis_label = NULL) {

  # 1. Tidy Eval ----
  meas_1_expr <- rlang::enquo(x_axis_var)
  meas_2_expr <- rlang::enquo(y_axis_var)
  grouping_var_expr <- rlang::enquo(grouping_var)

  # range vector, min, max, range and limit scalar for plot axis span ----
  if (missing(lsl) & missing(usl)) {
    range_tbl <- data %>%
      dplyr::summarize(range_max = range(!!(meas_1_expr), !!(meas_2_expr))[2],
                       range_min = range(!!(meas_1_expr), !!(meas_2_expr))[1])
  } else {
    range_tbl <- data %>%
      dplyr::summarize(range_max = range(!!(meas_1_expr),
                                         !!(meas_2_expr),
                                         lsl,
                                         usl)[2],
                       range_min = range(!!(meas_1_expr),
                                         !!(meas_2_expr),
                                         lsl,
                                         usl)[1])
  }


  min <- range_tbl %>%
    dplyr::pull(range_min)

  max <- range_tbl %>%
    dplyr::pull(range_max)

  range_vector <- abs(max - min)
  limit_scalar <- abs(range_vector / 6)


  # 2. Plotting function ----
  if (missing(grouping_var)) {

    plot <- data %>%

      ggplot2::ggplot(ggplot2::aes(!!(meas_1_expr), !!(meas_2_expr))) +
      ggplot2::geom_point(color  = "#3971CB", alpha  = alpha, size = 2) +

      ggplot2::theme_light() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::geom_abline(color = "grey") +
      ggplot2::coord_fixed(ratio = 1,
                           xlim  = c(min-limit_scalar, max+limit_scalar),
                           ylim  = c(min-limit_scalar, max+limit_scalar)) +
      ggplot2::labs(
        title    = "Youden Plot",
        subtitle = analysis_desc_label,
        x        = ifelse(is.null(x_axis_label), stringr::str_glue("{as_label(meas_1_expr)}"), x_axis_label),
        y        = ifelse(is.null(y_axis_label), stringr::str_glue("{as_label(meas_2_expr)}"), y_axis_label)) +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(hjust = 0.5, size = 18, color = "grey50"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "grey50"),
        axis.title.x  = ggplot2::element_text(size = 10, color = "grey50"),
        axis.title.y  = ggplot2::element_text(size = 10, color = "grey50"),
        axis.text     = ggplot2::element_text(size = 9, color = "grey50"))


    if (median_line) plot <- plot + ggplot2::geom_quantile(quantiles = 0.5,
                                                           size = 0.8,
                                                           linetype = "dashed",
                                                           color = "grey50")

  } else {

    plot <- data %>%

      dplyr::mutate(!!grouping_var_expr := forcats::as_factor(!!grouping_var_expr)) %>%

      ggplot2::ggplot(ggplot2::aes(!!(meas_1_expr), !!(meas_2_expr))) +
      ggplot2::geom_point(ggplot2::aes(color = !!(grouping_var_expr)), alpha  = alpha, size = size) +

      ggplot2::theme_light() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::geom_abline(color = "grey") +
      ggplot2::coord_fixed(ratio = 1,
                           xlim  = c(min-limit_scalar, max+limit_scalar),
                           ylim  = c(min-limit_scalar, max+limit_scalar)) +
      ggplot2::labs(
        title    = "Youden Plot",
        subtitle = analysis_desc_label,
        x        = ifelse(is.null(x_axis_label), stringr::str_glue("{as_label(meas_1_expr)}"), x_axis_label),
        y        = ifelse(is.null(y_axis_label), stringr::str_glue("{as_label(meas_2_expr)}"), y_axis_label)) +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(hjust = 0.5, size = 18, color = "grey50"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "grey50"),
        axis.title.x  = ggplot2::element_text(size = 10, color = "grey50"),
        axis.title.y  = ggplot2::element_text(size = 10, color = "grey50"),
        axis.text     = ggplot2::element_text(size = 9, color = "grey50"),
        legend.title  = ggplot2::element_text(color = "grey50", size = 11),
        legend.text   = ggplot2::element_text(color = "grey50", size = 11)) +
      sherlock::scale_color_sherlock()

    if (median_line) plot <- plot + ggplot2::geom_quantile(quantiles = 0.5,
                                                           size = 0.8,
                                                           linetype = "dashed",
                                                           color = "grey50")


    small_plots <- data %>%

      dplyr::mutate(!!grouping_var_expr := forcats::as_factor(!!grouping_var_expr)) %>%

      ggplot2::ggplot(ggplot2::aes(!!(meas_1_expr), !!(meas_2_expr))) +
      ggplot2::geom_point(ggplot2::aes(color = !!(grouping_var_expr)), alpha  = alpha, size = 3/4*size) +
      ggplot2::facet_wrap(ggplot2::vars(!!grouping_var_expr), ncol = 4) +

      ggplot2::theme_light() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::geom_abline(color = "grey") +
      ggplot2::coord_fixed(
        ratio = 1,
        xlim  = c(min-limit_scalar, max+limit_scalar),
        ylim  = c(min-limit_scalar, max+limit_scalar)) +
      ggplot2::labs(
        x        = "",
        y        = "") +
      ggplot2::theme(
        axis.title.x  = ggplot2::element_text(size = 8, color = "grey50"),
        axis.title.y  = ggplot2::element_text(size = 8, color = "grey50"),
        axis.text     = ggplot2::element_text(size = 8, color = "grey50"),
        legend.position  = "none",
        strip.background = ggplot2::element_rect(fill = "#FFFFFF", color = "grey"),
        strip.text       = ggplot2::element_text(size = 10, color = "grey50")) +
      sherlock::scale_color_sherlock()

    if (median_line) small_plots <- small_plots + ggplot2::geom_quantile(quantiles = 0.5,
                                                                         size = 0.8,
                                                                         linetype = "dashed",
                                                                         color = "grey50")

    plot <- cowplot::plot_grid(plot,
                               small_plots,
                               ncol = 1,
                               rel_heights = c(3, 1.5),
                               align       = "v",
                               axis = "lr")
  }



  if (missing(lsl) & missing(usl)) {
    plot
  } else {
    plot <- plot + ggplot2::geom_rect(ggplot2::aes(xmin = lsl, xmax = usl, ymin = lsl, ymax = usl), color = "#C8102E", fill = NA, size = 0.5)
  }


  return(plot)
}
