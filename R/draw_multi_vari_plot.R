#' Draw multi-vari plot
#'
#' @description
#' Draws a multi-vari small multiples plot
#'
#' @param data input dataset to be plotted (required)
#' @param response response variable, Y (required)
#' @param factor_1 lowest level factor (required)
#' @param factor_2 mid-level factor (required)
#' @param factor_3 top level factor (optional)
#' @param plot_means logical. if FALSE, default, means for mid-level factor are not plotted (optional)
#'
#' @return A ggplot multi-vari small multiples object
#'
#' @export


draw_multi_vari_plot <- function(data, response, factor_1, factor_2, factor_3, plot_means = FALSE) {

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
      panel.border      = ggh4x::element_part_rect(color = "grey95", size = 0.2),
      strip.background  = ggplot2::element_rect(fill = "white", color = "grey70", size = 0.5),
      strip.text        = ggplot2::element_text(size = 14, color = "grey50"),
      plot.title        = ggplot2::element_text(size = 20, color = "grey50"),
      plot.subtitle     = ggplot2::element_text(size = 14, color = "grey70"),
      axis.title.x      = ggplot2::element_text(size = 13, color = "grey70"),
      axis.title.y      = ggplot2::element_text(size = 13, color = "grey70"),
      axis.text.x       = ggplot2::element_text(size = 11, color = "grey70"),
      axis.text.y       = ggplot2::element_text(size = 12, color = "grey70"),
      axis.line.x.bottom        = ggplot2::element_line(color = "grey70", size = 0.5),
      axis.line.y.left  = ggplot2::element_line(color = "grey70", size = 0.5),
      axis.line.y.right = ggplot2::element_line(color = "grey70", size = 0.5),
      axis.text.y.right = ggplot2::element_blank(),
      legend.position   = "none",
      plot.caption      = ggplot2::element_text(color = "grey50")
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
        title = "Multi-Vari Chart",
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
        ggplot2::geom_point(size = 2.5, alpha = 0.6) +
        ggplot2::geom_line(ggplot2::aes(group = !!factor_1_expr), size = 0.7, alpha = 0.6)
    }


    # 4.2 if 3 factors ----
  } else {

    # Mean tbl for plotting averages ----
    mean_tbl <- data %>%
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
          title    = "Multi-Vari Chart",
          subtitle = stringr::str_glue("{as_label(factor_1_expr)} by {as_label(factor_2_expr)} by {as_label(factor_3_expr)}"),
          x        = stringr::str_glue("{as_label(factor_2_expr)}"),
          y        = stringr::str_glue("{as_label(response_expr)}"),
          caption  = stringr::str_glue("Blue data points represent averages for factor {as_label(factor_2_expr) }
                                       Grey data points represent individual values for factor {as_label(factor_1_expr)}")
        )

    } else {

      multi_vari_chart <- multi_vari_tbl %>%

        ggplot2::ggplot(ggplot2::aes(!!factor_1_expr, !!response_expr, color = !!factor_2_expr)) +
        ggplot2::geom_point(size = 2.5, alpha = 0.6) +
        ggplot2::geom_line(size = 0.7, group = 1, alpha = 0.6) +

        ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!factor_3_expr, !!factor_2_expr)) +
        ggplot2::guides(y.sec = "axis") +

        theme_element +
        ggplot2::labs(
          title    = "Multi-Vari Chart",
          subtitle = stringr::str_glue("{as_label(factor_1_expr)} by {as_label(factor_2_expr)} by {as_label(factor_3_expr)}"),
          x        = stringr::str_glue("{as_label(factor_1_expr)}"),
          y        = stringr::str_glue("{as_label(response_expr)}")
        )

    }

  }

  # 5. Scale color ----
  multi_vari_chart <- multi_vari_chart +
    ggplot2::scale_color_manual(values = c("#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A"))

  return(multi_vari_chart)

}
