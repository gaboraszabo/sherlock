#' Draw Multivari Plot
#'
#' @description
#' Draws a multivari small multiples plot
#'
#' @param data input dataset to be plotted (required)
#' @param response response variable, Y (required)
#' @param factor_1 lowest level factor (required)
#' @param factor_2 mid-level factor (required)
#' @param factor_3 top level factor (optional)
#' @param plot_means logical. if FALSE, default, means for mid-level factor are not plotted (optional)
#' @param point_size Set point size. By default, it is set to 2.5  (optional)
#' @param line_size Set line size. By default, it is set to 0.7  (optional)
#' @param alpha Set transparency. By default, it is set to 0.6  (optional)
#' @param x_axis_text_size set x axis text size. options are "normal" (default), "small", "xs" and "none" (optional)
#' @param panel_text_size set panel text size. By default, it is set to 14 (optional)
#'
#' @return A 'ggplot' object
#'
#' @examples
#' library(dplyr)
#' library(ggh4x)
#'
#' polar_small_multiples_data %>%
#'   filter(ID_Measurement_Angle %in% c(0, 45, 90, 135)) %>%
#'   normalize_observations(response = ID,
#'                          grouping_var = Tip_Bottom,
#'                          ref_values = c(0.2075, 0.2225)) %>%
#'   draw_multivari_plot(response    = ID_normalized,
#'                      factor_1    = ID_Measurement_Angle,
#'                      factor_2    = Mold_Cavity_Number,
#'                      factor_3    = Tip_Bottom,
#'                      x_axis_text = 6) +
#'   draw_horizontal_reference_line(reference_line = 0)
#'
#' @export


draw_multivari_plot <- function(data, response, factor_1, factor_2, factor_3, plot_means = FALSE,
                                x_axis_text_size = 11, panel_text_size = 14, point_size = 2.5, line_size = 0.7, alpha = 0.6) {

  # 1. Tidy Eval ----
  response_expr <- rlang::enquo(response)
  factor_1_expr <- rlang::enquo(factor_1)
  factor_2_expr <- rlang::enquo(factor_2)
  factor_3_expr <- rlang::enquo(factor_3)

  # 2. Theme element ----
  theme_element <- ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.spacing     = ggplot2::unit(0, "lines"),
      panel.border      = ggh4x::element_part_rect(color = "grey95", linewidth = 0.2),
      strip.background  = ggplot2::element_rect(fill = "white", color = "grey70", size = 0.5),
      strip.text        = ggplot2::element_text(size = panel_text_size, color = "grey50"),
      plot.title        = ggplot2::element_text(size = 20, color = "grey50"),
      plot.subtitle     = ggplot2::element_text(size = 14, color = "grey70"),
      axis.title.x      = ggplot2::element_text(size = 13, color = "grey70"),
      axis.title.y      = ggplot2::element_text(size = 13, color = "grey70"),
      axis.text.x       = ggplot2::element_text(size = x_axis_text_size, color = "grey70"),
      axis.text.y       = ggplot2::element_text(size = 12, color = "grey70"),
      axis.line.x.bottom        = ggplot2::element_line(color = "grey70", size = 0.5),
      axis.line.y.left  = ggplot2::element_line(color = "grey70", size = 0.5),
      axis.line.y.right = ggplot2::element_line(color = "grey70", size = 0.5),
      axis.text.y.right = ggplot2::element_blank(),
      legend.position   = "none",
      plot.caption      = ggplot2::element_text(color = "grey50"),
      axis.ticks        = ggplot2::element_blank()
    )

  # 3. Warning messages ----
  if (missing(factor_2) & missing(factor_3)) message("You need two specify at least two factors (factor_1 and factor_2)!")


  # 4. Conditionals ----
  # 4.1 if only 2 factors ----
  if (missing(factor_3)) {

    # Mean tbl for plotting averages ----
    mean_tbl <- data %>%
      dplyr::mutate(!!as_label(factor_2_expr) := forcats::as_factor(!!factor_2_expr)) %>%
      dplyr::mutate(!!as_label(factor_1_expr) := forcats::as_factor(!!factor_1_expr)) %>%
      dplyr::group_by(!!factor_2_expr, !!factor_1_expr) %>%
      dplyr::summarize(mean = mean(!!response_expr)) %>%
      dplyr::mutate(factor_2 := !!factor_2_expr)

    # Multi-vari tbl ----
    multi_vari_tbl <- data %>%
      dplyr::mutate(!!(factor_2_expr) := forcats::as_factor(!!factor_2_expr)) %>%
      dplyr::mutate(!!(factor_1_expr) := forcats::as_factor(!!factor_1_expr))


    # Plotting function ----
    multi_vari_chart <- multi_vari_tbl %>%

      ggplot2::ggplot(ggplot2::aes(!!factor_1_expr, !!response_expr, color = !!factor_1_expr)) +
      ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!factor_2_expr)) +
      ggplot2::guides(y.sec = "axis") +
      theme_element +
      ggplot2::labs(
        title = "Multivari Plot",
        subtitle = stringr::str_glue("{as_label(factor_1_expr)} by {as_label(factor_2_expr)}"),
        x = stringr::str_glue("{as_label(factor_1_expr)}"),
        y = stringr::str_glue("{as_label(response_expr)}")
      )

    if (plot_means) {
      multi_vari_chart <- multi_vari_chart +
        ggplot2::geom_point(color = "grey30", size = 1.5, alpha = 0.4) +
        ggplot2::geom_point(data = mean_tbl, ggplot2::aes(!!factor_1_expr, mean), color = "#40506e", size = 3, alpha = 0.7) +
        ggplot2::geom_line(data = mean_tbl, ggplot2::aes(!!factor_1_expr, mean, group = 1), color = "#40506e", alpha = 0.7) +
        ggplot2::labs(caption  = stringr::str_glue("Blue data points represent averages for factor {as_label(factor_1_expr)}"))
    } else {
      multi_vari_chart <- multi_vari_chart +
        ggplot2::geom_point(size = point_size, alpha = alpha) +
        ggplot2::geom_line(ggplot2::aes(group = !!factor_1_expr), size = line_size, alpha = alpha)
    }


    # 4.2 if 3 factors ----
  } else {

    # Mean tbl for plotting averages ----
    mean_tbl <- data %>%
      dplyr::mutate(!!(factor_3_expr) := forcats::as_factor(!!factor_3_expr)) %>%
      dplyr::mutate(!!as_label(factor_2_expr) := forcats::as_factor(!!factor_2_expr)) %>%
      dplyr::mutate(!!as_label(factor_1_expr) := forcats::as_factor(!!factor_1_expr)) %>%
      dplyr::group_by(!!factor_3_expr, !!factor_2_expr) %>%
      dplyr::summarize(mean = mean(!!response_expr)) %>%
      dplyr::mutate(factor_2 := !!factor_2_expr)

    # Multi-vari tbl ----
    multi_vari_tbl <- data %>%
      dplyr::mutate(!!(factor_3_expr) := forcats::as_factor(!!factor_3_expr)) %>%
      dplyr::mutate(!!(factor_2_expr) := forcats::as_factor(!!factor_2_expr)) %>%
      dplyr::mutate(!!(factor_1_expr) := forcats::as_factor(!!factor_1_expr))

    # Plotting function ----
    if (plot_means) {

      multi_vari_chart <- multi_vari_tbl %>%
        ggplot2::ggplot(ggplot2::aes(!!factor_2_expr, !!response_expr)) +
        ggplot2::geom_point(color = "grey30", size = 1.5, alpha = 0.4) +
        ggplot2::geom_point(data = mean_tbl, ggplot2::aes(!!factor_2_expr, mean), color = "#40506e", size = 3, alpha = 0.7) +
        ggplot2::geom_line(data = mean_tbl, ggplot2::aes(!!factor_2_expr, mean, group = 1), color = "#40506e", alpha = 0.7) +

        ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!factor_3_expr)) +
        ggplot2::guides(y.sec = "axis") +

        theme_element +
        ggplot2::labs(
          title    = "Multivari Plot",
          subtitle = stringr::str_glue("{as_label(factor_1_expr)} by {as_label(factor_2_expr)} by {as_label(factor_3_expr)}"),
          x        = stringr::str_glue("{as_label(factor_2_expr)}"),
          y        = stringr::str_glue("{as_label(response_expr)}"),
          caption  = stringr::str_glue("Blue data points represent averages for factor {as_label(factor_2_expr) }
                                       Grey data points represent individual values for factor {as_label(factor_1_expr)}")
        )

    } else {

      multi_vari_chart <- multi_vari_tbl %>%

        ggplot2::ggplot(ggplot2::aes(!!factor_1_expr, !!response_expr, color = !!factor_2_expr)) +
        ggplot2::geom_point(size = point_size, alpha = alpha) +
        ggplot2::geom_line(size = line_size, group = 1, alpha = alpha) +

        ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!factor_3_expr, !!factor_2_expr)) +
        ggplot2::guides(y.sec = "axis") +

        theme_element +
        ggplot2::labs(
          title    = "Multivari Plot",
          subtitle = stringr::str_glue("{as_label(factor_1_expr)} by {as_label(factor_2_expr)} by {as_label(factor_3_expr)}"),
          x        = stringr::str_glue("{as_label(factor_1_expr)}"),
          y        = stringr::str_glue("{as_label(response_expr)}")
        )

    }

  }

  # 5. Scale color ----
  multi_vari_chart <- multi_vari_chart +
    sherlock::scale_color_sherlock()


  return(multi_vari_chart)

}
