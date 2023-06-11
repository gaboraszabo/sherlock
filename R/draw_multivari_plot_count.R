#' Draw Multivari Plot for Counts
#'
#' @description
#' Draws a multivari small multiples plot for count data
#'
#' @param data Input dataset to be plotted (required)
#' @param y_var Response variable, Y (required)
#' @param grouping_var_1 Select column for lowest level grouping variable (required)
#' @param grouping_var_2 Select column for second level grouping variable (required)
#' @param grouping_var_3 Select column for third level grouping variable (optional)
#' @param grouping_var_4 Select column for fourth level grouping variable (optional)
#' @param alpha Set transparency. By default, it is set to 0.6  (optional)
#' @param x_axis_text_size Set X axis text size. By default, it is set to 11  (optional)
#' @param panel_text_size Set panel text size. By default, it is set to 14 (optional)
#'
#' @return A 'ggplot' object


draw_multivari_plot_count <- function(data, y_var, grouping_var_1, grouping_var_2, grouping_var_3, grouping_var_4,
                                      x_axis_text_size = 11, panel_text_size = 14, alpha = 0.6) {

  # 1. Tidy Eval ----
  response_expr   <- rlang::enquo(y_var)
  factor_1_expr   <- rlang::enquo(grouping_var_1)
  factor_2_expr   <- rlang::enquo(grouping_var_2)
  factor_3_expr   <- rlang::enquo(grouping_var_3)
  factor_4_expr   <- rlang::enquo(grouping_var_4)

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
      plot.caption      = ggplot2::element_text(color = "grey60"),
      axis.ticks        = ggplot2::element_blank()
    )

  # 3. Warning messages ----
  if (missing(grouping_var_2) & missing(grouping_var_3)) message("You need two specify at least two grouping variables (grouping_var_1 and grouping_var_2)!")

  if (!((data %>% dplyr::pull(!!response_expr)) %>% is.numeric())) {
    message("Response variable is not numeric. Are you sure this is what you meant to do?")
  }


  # 4. Conditionals ----
  # 4.1 if only 2 factors ----
  if (missing(grouping_var_3)) {

    # Multi-vari tbl ----
    multi_vari_tbl <- data %>%
      dplyr::mutate(!!(factor_2_expr) := forcats::as_factor(!!factor_2_expr)) %>%
      dplyr::mutate(!!(factor_1_expr) := forcats::as_factor(!!factor_1_expr))


    # Plotting function ----
    multi_vari_chart <- multi_vari_tbl %>%

      ggplot2::ggplot(ggplot2::aes(!!factor_1_expr, !!response_expr)) +
      ggplot2::geom_col(alpha = alpha, fill = sherlock::scale_fill_sherlock(palette = 3)) +
      ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!factor_2_expr), scales = "free_x",
                          space = "free_x") +
      ggplot2::guides(y.sec = "axis") +
      theme_element +
      ggplot2::labs(
        title = "Multivari Plot",
        subtitle = stringr::str_glue("{as_label(factor_1_expr)} by {as_label(factor_2_expr)}"),
        x = stringr::str_glue("{as_label(factor_1_expr)}"),
        y = stringr::str_glue("{as_label(response_expr)}")
      )
  }


  # 4.2 if 3 factors ----
  if (!missing(grouping_var_3) & missing(grouping_var_4)) {

    # Multi-vari tbl ----
    multi_vari_tbl <- data %>%
      dplyr::mutate(!!(factor_3_expr) := forcats::as_factor(!!factor_3_expr)) %>%
      dplyr::mutate(!!(factor_2_expr) := forcats::as_factor(!!factor_2_expr)) %>%
      dplyr::mutate(!!(factor_1_expr) := forcats::as_factor(!!factor_1_expr))

    # Plotting function ----
    multi_vari_chart <- multi_vari_tbl %>%

      ggplot2::ggplot(ggplot2::aes(!!factor_1_expr, !!response_expr)) +
      ggplot2::geom_col(alpha = alpha, fill = sherlock::scale_fill_sherlock(palette = 3)) +

      ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!factor_3_expr, !!factor_2_expr),
                          scales = "free_x",
                          space = "free_x") +
      ggplot2::guides(y.sec = "axis") +

      theme_element +
      ggplot2::labs(
        title    = "Multivari Plot",
        subtitle = stringr::str_glue("{as_label(factor_1_expr)} by {as_label(factor_2_expr)} by {as_label(factor_3_expr)}"),
        x        = stringr::str_glue("{as_label(factor_1_expr)}"),
        y        = stringr::str_glue("{as_label(response_expr)}")
      )
  }


  # 4.2 if 4 factors ----
  if (!missing(grouping_var_3) & !missing(grouping_var_4)) {

    # Multi-vari tbl ----
    multi_vari_tbl <- data %>%
      dplyr::mutate(!!(factor_4_expr) := forcats::as_factor(!!factor_4_expr)) %>%
      dplyr::mutate(!!(factor_3_expr) := forcats::as_factor(!!factor_3_expr)) %>%
      dplyr::mutate(!!(factor_2_expr) := forcats::as_factor(!!factor_2_expr)) %>%
      dplyr::mutate(!!(factor_1_expr) := forcats::as_factor(!!factor_1_expr))

    # Plotting function ----
    multi_vari_chart <- multi_vari_tbl %>%

      ggplot2::ggplot(ggplot2::aes(!!factor_1_expr, !!response_expr)) +
      ggplot2::geom_col(alpha = alpha, fill = sherlock::scale_fill_sherlock(palette = 3)) +

      ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!factor_4_expr,
                                                                       !!factor_3_expr,
                                                                       !!factor_2_expr),
                          scales = "free_x",
                          space = "free_x") +
      ggplot2::guides(y.sec = "axis") +

      theme_element +
      ggplot2::labs(
        title    = "Multivari Plot",
        subtitle = stringr::str_glue("{as_label(factor_1_expr)} by {as_label(factor_2_expr)} by {as_label(factor_3_expr)} by {as_label(factor_4_expr)}"),
        x        = stringr::str_glue("{as_label(factor_1_expr)}"),
        y        = stringr::str_glue("{as_label(response_expr)}")
      )

  }


  return(multi_vari_chart)

}
