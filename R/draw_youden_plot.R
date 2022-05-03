#' Draw Youden Plot
#'
#' @description
#' Draws a Youden Plot
#'
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#'
#' @return A ggplot Youden Plot object
#'
#' @export

draw_youden_plot <- function(data, x_axis_var, y_axis_var, lsl, usl,
                             grouping_var, median_line = FALSE, analysis_desc_label = NULL,
                             x_axis_label = "Measurement 1", y_axis_label = "Measurement 2") {

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


  # plot ----
  if (missing(grouping_var)) {

    plot <- data %>%

      ggplot2::ggplot(aes(!!(meas_1_expr), !!(meas_2_expr))) +
      ggplot2::geom_point(color  = "#3971CB", alpha  = 0.4, size = 2.5) +

      ggplot2::theme_light() +
      ggplot2::theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
      ggplot2::geom_abline(color = "grey") +
      ggplot2::coord_fixed(ratio = 1,
                           xlim  = c(min-limit_scalar, max+limit_scalar),
                           ylim  = c(min-limit_scalar, max+limit_scalar)) +
      ggplot2::labs(
        title    = "Youden Plot",
        subtitle = analysis_desc_label,
        x        = x_axis_label,
        y        = y_axis_label) +
      ggplot2::theme(
        plot.title    = element_text(hjust = 0.5, size = 20, color = "grey30"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, color = "grey30"),
        axis.title.x  = element_text(size = 13, color = "grey30"),
        axis.title.y  = element_text(size = 13, color = "grey30"),
        axis.text     = element_text(size = 12, color = "grey30"))

    if (median_line) plot <- plot + ggplot2::geom_quantile(quantiles = 0.5,
                                                           size = 0.8,
                                                           linetype = "dashed",
                                                           color = "grey50")

  } else {

    plot <- data %>%

      dplyr::mutate(!!grouping_var_expr := as_factor(!!grouping_var_expr)) %>%

      ggplot2::ggplot(aes(!!(meas_1_expr), !!(meas_2_expr))) +
      ggplot2::geom_point(aes(color = !!(grouping_var_expr)), alpha  = 0.4, size = 2.5) +

      ggplot2::theme_light() +
      ggplot2::theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
      ggplot2::geom_abline(color = "grey") +
      ggplot2::coord_fixed(ratio = 1,
                           xlim  = c(min-limit_scalar, max+limit_scalar),
                           ylim  = c(min-limit_scalar, max+limit_scalar)) +
      ggplot2::labs(
        title    = "Youden Plot",
        subtitle = analysis_desc_label,
        x        = x_axis_label,
        y        = y_axis_label) +
      ggplot2::theme(
        plot.title    = element_text(hjust = 0.5, size = 20, color = "grey30"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, color = "grey30"),
        axis.title.x  = element_text(size = 13, color = "grey30"),
        axis.title.y  = element_text(size = 13, color = "grey30"),
        axis.text     = element_text(size = 12, color = "grey30")) +
      ggplot2::scale_color_manual(values = c("#3971CB", "#D76213", "#111111", "#9A0000",
                                             "#335F34", "#8E5816", "#624187", "#141B7A"))

    if (median_line) plot <- plot + ggplot2::geom_quantile(quantiles = 0.5,
                                                           size = 0.8,
                                                           linetype = "dashed",
                                                           color = "grey50")


    small_plots <- data %>%

      dplyr::mutate(!!grouping_var_expr := as_factor(!!grouping_var_expr)) %>%

      ggplot2::ggplot(aes(!!(meas_1_expr), !!(meas_2_expr))) +
      ggplot2::geom_point(aes(color = !!(grouping_var_expr)), alpha  = 0.4, size = 1.5) +
      ggplot2::facet_wrap(vars(!!grouping_var_expr), ncol = 4) +

      ggplot2::theme_light() +
      ggplot2::theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
      ggplot2::geom_abline(color = "grey") +
      ggplot2::coord_fixed(
        ratio = 1,
        xlim  = c(min-limit_scalar, max+limit_scalar),
        ylim  = c(min-limit_scalar, max+limit_scalar)) +
      ggplot2::labs(
        x        = x_axis_label,
        y        = y_axis_label) +
      ggplot2::theme(
        axis.title.x  = element_text(size = 10, color = "grey30"),
        axis.title.y  = element_text(size = 10, color = "grey30"),
        axis.text     = element_text(size = 8, color = "grey30"),
        legend.position  = "none",
        strip.background = element_rect(fill = "#FFFFFF", color = "grey"),
        strip.text       = element_text(size = 10, color = "grey30")) +
      ggplot2::scale_color_manual(values = c("#3971CB", "#D76213", "#111111", "#9A0000",
                                             "#335F34", "#8E5816", "#624187", "#141B7A"))

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
    plot <- plot + ggplot2::geom_rect(aes(xmin = lsl, xmax = usl, ymin = lsl, ymax = usl), color = "#C8102E", fill = NA, size = 0.5)
  }


  return(plot)
}
