#' Draw Categorical Scatterplot
#'
#' @description
#' Draws a Categorical Scatterplot
#'
#' @param data input dataset to be plotted (required)
#' @param y_var Y variable to be plotted on Y axis (required)
#' @param grouping_var_1 First grouping variable (optional)
#' @param grouping_var_2 Second, higher-level grouping variable  (optional)
#' @param grouping_var_3 Third, highest-level grouping variable (optional)
#' @param group_color  Set whether to color by grouping_var_1. By default, it is set to FALSE (optional)
#' @param size Set point size. By default, it is set to 2  (optional)
#' @param alpha Set transparency. By default, it is set to 0.5  (optional)
#' @param jitter Set whether to add jitter. By default, it is set to TRUE  (optional)
#' @param interactive Set plot interactivity. By default, it is set to FALSE (optional)
#'
#' @return A 'ggplot' or 'plotly' object
#'
#' @examples
#' multi_vari_data_2 %>%
#'    draw_categorical_scatterplot(y_var = Length,
#'                                 grouping_var_1 = Part,
#'                                 grouping_var_2 = Operator,
#'                                 jitter = FALSE)
#'
#' @export

draw_categorical_scatterplot <- function(data, y_var, grouping_var_1, grouping_var_2, grouping_var_3,
                                     group_color = FALSE, size = 2, alpha = 0.5, jitter = TRUE, interactive = FALSE) {

  # 0. MESSAGES AND WARNINGS ----
  if (missing(y_var)) {
    stop("You must specify y_var!")
  }

  if (!missing(grouping_var_1) && missing(grouping_var_2) && !missing(grouping_var_3)) {
    stop("Did you mean to specify grouping_var_2 instead of grouping_var_3?\nYou must specify grouping variables in the right order, i.e. grouping_var_1, grouping_var_2 and then grouping_var_3.")
  }


  # 1. Tidy Eval ----
  y_var_expr <- rlang::enquo(y_var)
  grouping_var_1_expr <- rlang::enquo(grouping_var_1)
  grouping_var_2_expr <- rlang::enquo(grouping_var_2)
  grouping_var_3_expr <- rlang::enquo(grouping_var_3)


  # 2. Data Transformation ----
  if (missing(grouping_var_1) && missing(grouping_var_2) && missing(grouping_var_2)) {
    data <- data %>%
      dplyr::mutate(variable = "variable")
  }

  if (!missing(grouping_var_1)) {
    data <- data %>%
      dplyr::mutate(!!grouping_var_1_expr := !!grouping_var_1_expr %>% forcats::as_factor())
  }

  if (!missing(grouping_var_2)) {
    data <- data %>%
      dplyr::mutate(!!grouping_var_2_expr := !!grouping_var_2_expr %>% forcats::as_factor())
  }

  if (!missing(grouping_var_3)) {
    data <- data %>%
      dplyr::mutate(!!grouping_var_3_expr := !!grouping_var_3_expr %>% forcats::as_factor())
  }




  # 3. Plotting ----
  if (missing(grouping_var_1) && missing(grouping_var_2)) {
    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(variable, !!y_var_expr)) +
      ggplot2::geom_jitter(color = sherlock::scale_color_sherlock(2), fill = sherlock::scale_fill_sherlock(2), alpha = alpha,
                           width = if (jitter) 0.015 else 0, shape = 21, size = size)
  }

  if (!missing(grouping_var_1)) {
    if (!group_color) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, group = !!grouping_var_1_expr)) +
        ggplot2::geom_jitter(color = sherlock::scale_color_sherlock(2), fill = sherlock::scale_fill_sherlock(2), alpha = alpha,
                             width = if (jitter) 0.03 else 0, shape = 21, size = size)
    }
    if (group_color) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_1_expr)) +
        ggplot2::geom_jitter(alpha = alpha, width = if (jitter) 0.03 else 0, size = size) +
        sherlock::scale_color_sherlock() +
        sherlock::scale_fill_sherlock()
    }
  }


  if(!missing(grouping_var_1) && !missing(grouping_var_2)) {
    if (!group_color) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, group = !!grouping_var_1_expr)) +
        ggplot2::geom_jitter(color = sherlock::scale_color_sherlock(2), fill = sherlock::scale_fill_sherlock(2), alpha = alpha,
                             width = if (jitter) 0.03 else 0, shape = 21, size = size) +
        ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!grouping_var_2_expr))
    }
    if (group_color) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_1_expr)) +
        ggplot2::geom_jitter(alpha = alpha, width = if (jitter) 0.03 else 0, size = size) +
        ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!grouping_var_2_expr)) +
        sherlock::scale_color_sherlock() +
        sherlock::scale_fill_sherlock()
    }
  }


  if(!missing(grouping_var_1) && !missing(grouping_var_2) && !missing(grouping_var_3)) {
    if (!group_color) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, group = !!grouping_var_1_expr)) +
        ggplot2::geom_jitter(color = sherlock::scale_color_sherlock(2), fill = sherlock::scale_fill_sherlock(2), alpha = alpha,
                             width = if (jitter) 0.03 else 0, shape = 21, size = size) +
        ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!grouping_var_3_expr, !!grouping_var_2_expr))
    }
    if (group_color) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_1_expr)) +
        ggplot2::geom_jitter(alpha = alpha, width = if (jitter) 0.03 else 0, size = size) +
        ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!grouping_var_3_expr, !!grouping_var_2_expr)) +
        sherlock::scale_color_sherlock() +
        sherlock::scale_fill_sherlock()
    }
  }


  # Theme ----

  if (missing(grouping_var_1)) {
    plot <- plot +
      sherlock::theme_sherlock() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x  = ggplot2::element_blank())
  }

  if (!missing(grouping_var_1)) {
    plot <- plot +
      sherlock::theme_sherlock()
  }


  paneled_theme_element <-
    ggplot2::theme(
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.spacing     = ggplot2::unit(0, "lines"),
      panel.border      = ggh4x::element_part_rect(color = "grey95", linewidth = 0.2),
      strip.background  = ggplot2::element_rect(fill = "white", color = "grey70", size = 0.5),
      strip.text        = ggplot2::element_text(size = 11, color = "grey50"),
      plot.title        = ggplot2::element_text(size = 14, color = "grey50"),
      plot.subtitle     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.title.x      = ggplot2::element_text(size = 12, color = "grey50"),
      axis.title.y      = ggplot2::element_text(size = 12, color = "grey50"),
      axis.text.x       = ggplot2::element_text(size = 11, color = "grey50"),
      axis.text.y       = ggplot2::element_text(size = 12, color = "grey50"),
      axis.line.x.bottom        = ggplot2::element_line(color = "grey70", size = 0.5),
      axis.line.y.left  = ggplot2::element_line(color = "grey50", size = 0.5),
      axis.line.y.right = ggplot2::element_line(color = "grey50", size = 0.5),
      axis.text.y.right = ggplot2::element_blank(),
      legend.position   = "none",
      plot.caption      = ggplot2::element_text(color = "grey50"),
      axis.ticks        = ggplot2::element_blank()
    )

  #plot <- plot + paneled_theme_element


  if(!missing(grouping_var_2) && missing(grouping_var_3)) {
    plot <- plot + paneled_theme_element
  }

  if(!missing(grouping_var_2) && !missing(grouping_var_3)) {
    plot <- plot + paneled_theme_element
  }


  # 4. Labs ----

  if (missing(grouping_var_1) && missing(grouping_var_2) && missing(grouping_var_3)) {
    plot <- plot +
      ggplot2::labs(title = "Scatterplot")
  }

  if (!missing(grouping_var_1) && missing(grouping_var_2) && missing(grouping_var_3)) {
    plot <- plot +
      ggplot2::labs(title = "Grouped Scatterplot",
                    subtitle = stringr::str_glue("By {as_label(grouping_var_1_expr)}"))
  }

  if (!missing(grouping_var_1) && !missing(grouping_var_2) && missing(grouping_var_3)) {
    plot <- plot +
      ggplot2::labs(title = "Grouped Scatterplot",
                    subtitle = stringr::str_glue("{as_label(grouping_var_1_expr)} by {as_label(grouping_var_2_expr)}"))
  }

  if (!missing(grouping_var_1) && !missing(grouping_var_2) && !missing(grouping_var_3)) {
    plot <- plot +
      ggplot2::labs(title = "Grouped Scatterplot",
                    subtitle = stringr::str_glue("{as_label(grouping_var_1_expr)} by {as_label(grouping_var_2_expr)} by {as_label(grouping_var_3_expr)}"))
  }




  # if (missing(grouping_var_1) && missing(grouping_var_2) && missing(grouping_var_3)) {
  #   plot <- plot +
  #     ggplot2::labs(title = "Scatterplot")
  # } else {
  #   plot <- plot +
  #     ggplot2::labs(title = "Grouped Scatterplot")
  # }



  # 5. Interactivity ----
  if (interactive) {
    plot <- plotly::ggplotly(plot)
  }

  return(plot)

}
