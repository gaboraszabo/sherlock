#' Draw Small Multiples Line Plot
#'
#' @description
#' Draws a Small Multiples Line Plot
#'
#' @param data Input dataset to be plotted (required)
#' @param x_axis_var Variable to be plotted on x axis (required)
#' @param y_axis_var Variable to be plotted on x axis (required)
#' @param grouping_var Set grouping variable (required)
#' @param color_var Set variable to color by (optional)
#' @param faceting_var_1 Set first faceting variable (optional)
#' @param faceting_var_2 Set second faceting variable (optional)
#' @param plot_max_values Highlights maximum values per group. By default, it is set to FALSE (optional)
#' @param unique_color_by_group Set whether to display each group in a unique color. By default, it is set to FALSE (optional)
#' @param size Set line size. By default, it is set to 0.7  (optional)
#' @param alpha Set transparency. By default, it is set to 0.4  (optional)
#' @param interactive set plot interactivity. By default, it is set to TRUE (optional)
#' @param analysis_desc_label Label (subtitle) for analysis description. By default, it is set to NULL  (optional)
#' @param x_axis_label Label for x axis. By default, it is set to display x axis column name  (optional)
#' @param y_axis_label Label for y axis. By default, it is set to display y axis column name  (optional)
#' @param n_breaks_x_axis Set number of breaks on X axis. By default, it is set to 10 (optional)
#' @param n_breaks_y_axis Set number of breaks on Y axis. By default, it is set to 10 (optional)
#' @param accuracy Set number of decimal places to be displayed on X and Y axes. Examples: 0.1 - one decimal place, 0.01 - two decimal places, 0.001 - three decimal places etc. By default, it is set to 0.01 (optional)
#'
#' @return A 'ggplot' or 'plotly' object
#'
#' @export


draw_small_multiples_line_plot <- function(data, x_axis_var, y_axis_var, grouping_var, color_var,
                                           faceting_var_1, faceting_var_2, plot_max_values = FALSE,
                                           unique_color_by_group = FALSE,
                                           size = 0.7, alpha = 0.4, interactive = TRUE,
                                           analysis_desc_label = NULL, x_axis_label = NULL, y_axis_label = NULL,
                                           n_breaks_x_axis = 10, n_breaks_y_axis = 10, accuracy = 0.01) {

  # 1. Tidy Eval ----
  x_axis_var_expr <- rlang::enquo(x_axis_var)
  y_axis_var_expr <- rlang::enquo(y_axis_var)
  grouping_var_expr <- rlang::enquo(grouping_var)
  color_var_expr <- rlang::enquo(color_var)
  faceting_var_1_expr <- rlang::enquo(faceting_var_1)
  faceting_var_2_expr <- rlang::enquo(faceting_var_2)

  # 2.1 Data transformation for plot_max_values arg ----
  if (!plot_max_values) {
    data <- data %>%
      dplyr::mutate(!!grouping_var_expr := forcats::as_factor(!!grouping_var_expr))
  }

  if (plot_max_values) {
    data <- data %>%
      dplyr::mutate(!!grouping_var_expr := forcats::as_factor(!!grouping_var_expr)) %>%
      dplyr::group_by({{grouping_var}}, {{faceting_var_1}}, {{faceting_var_2}}) %>%
      dplyr::mutate(max = max({{y_axis_var}})) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = dplyr::case_when({{y_axis_var}} == max ~ max)) %>%
      dplyr::mutate(point_size = dplyr::case_when({{y_axis_var}} == max ~ 3)) %>%
      dplyr::mutate(point_stroke = dplyr::case_when({{y_axis_var}} == max ~ 2)) %>%
      dplyr::mutate(point_shape = dplyr::case_when({{y_axis_var}} == max ~ 21) %>% forcats::as_factor())
  }


  # 3. Plotting Function ----

  # 3.1 No color variable selected ----
  if(missing(color_var)) {

    if (unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_path(ggplot2::aes(color = !!grouping_var_expr), alpha = alpha, size = size)

      if (plot_max_values) {
        plot <- plot +
          ggplot2::geom_point(ggplot2::aes(x = !!x_axis_var_expr, y = value), size = data$point_size,
                              color = scale_color_sherlock(3), stroke = ifelse(interactive, 0.5, 1),
                              shape = data$point_shape, alpha = 1)
      }
    }

    if (!unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_path(color = "grey60", alpha = alpha, size = size)

      if (plot_max_values) {
        plot <- plot +
          ggplot2::geom_point(ggplot2::aes(x = !!x_axis_var_expr, y = value), size = data$point_size,
                              stroke = ifelse(interactive, 0.5, 1),
                              shape = data$point_shape, color = "grey50", alpha = 1)
      }
    }

  }

  # 3.2 Color variable selected ----
  if(!missing(color_var)) {

    if (unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_path(ggplot2::aes(color = !!color_var_expr), alpha = alpha, size = size)

      if (plot_max_values) {
        plot <- plot +
          ggplot2::geom_point(ggplot2::aes(x = !!x_axis_var_expr, y = value, color = !!color_var_expr),
                              size = data$point_size, stroke = ifelse(interactive, 0.5, 1),
                              shape = data$point_shape, alpha = 1)
      }
    }

    if (!unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_path(ggplot2::aes(color = !!color_var_expr), alpha = alpha, size = size)

      if (plot_max_values) {
        plot <- plot +
          ggplot2::geom_point(ggplot2::aes(x = !!x_axis_var_expr, y = value, color = !!color_var_expr),
                              size = data$point_size,
                              stroke = ifelse(interactive, 0.5, 1),
                              shape = data$point_shape, alpha = 1)
      }
    }
  }



  # 3.1 Faceting ----
  if (!missing(faceting_var_1) & missing(faceting_var_2)) {
    plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(!!faceting_var_1_expr))
  }


  if (!missing(faceting_var_1) & !missing(faceting_var_2)) {
    plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(!!faceting_var_1_expr),
                                       cols = ggplot2::vars(!!faceting_var_2_expr))
  }

  if (missing(faceting_var_1) & !missing(faceting_var_2)) {
    plot <- plot + ggplot2::facet_grid(cols = ggplot2::vars(!!faceting_var_2_expr))
  }



  # 4. Plot color scheme ----
  plot <- plot +
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(color = "grey50", face = "bold", size = 11),
      panel.border     = ggplot2::element_rect(colour = "grey70"),
      panel.spacing    = ggplot2::unit(1, "lines"),
      axis.title.x     = ggplot2::element_text(size = 10, color = "grey50"),
      axis.title.y     = ggplot2::element_text(size = 10, color = "grey50"),
      axis.text        = ggplot2::element_text(size = 9, color = "grey50"),
      legend.title     = ggplot2::element_text(color = "grey50", size = 11),
      legend.text      = ggplot2::element_text(color = "grey50", size = 11),
      plot.title       = ggplot2::element_text(hjust = 0, size = 16, color = "grey50"),
      plot.subtitle    = ggplot2::element_text(hjust = 0, size = 12, color = "grey50")) +
    sherlock::scale_color_sherlock()


  # 5. Labels ----
  if (interactive) {
    if (is.null(analysis_desc_label)) {
      plot <- plot +
        ggplot2::labs(title = "Small Multiples Line Plot",
                      x = ifelse(is.null(x_axis_label), stringr::str_glue("{as_label(x_axis_var_expr)}"), x_axis_label),
                      y = ifelse(is.null(y_axis_label), stringr::str_glue("{as_label(y_axis_var_expr)}"), y_axis_label))

    } else {
      plot <- plot +
        ggplot2::labs(
          title = stringr::str_glue("Small Multiples Line Plot - {analysis_desc_label}"),
          x = ifelse(is.null(x_axis_label), stringr::str_glue("{as_label(x_axis_var_expr)}"), x_axis_label),
          y = ifelse(is.null(y_axis_label), stringr::str_glue("{as_label(y_axis_var_expr)}"), y_axis_label)
        ) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 12, color = "grey50"))
    }

  } else {
    plot <- plot +
      ggplot2::labs(
        title = "Small Multiples Line Plot",
        x = ifelse(is.null(x_axis_label), stringr::str_glue("{as_label(x_axis_var_expr)}"), x_axis_label),
        y = ifelse(is.null(y_axis_label), stringr::str_glue("{as_label(y_axis_var_expr)}"), y_axis_label),
        subtitle = analysis_desc_label
      )
  }


  # 6. X-Y axis ----
  plot <- plot +
    ggplot2::scale_x_continuous(n.breaks = n_breaks_x_axis, labels = scales::number_format(accuracy = accuracy)) +
    ggplot2::scale_y_continuous(n.breaks = n_breaks_y_axis, labels = scales::number_format(accuracy = accuracy))



  # 7. Interactivity with ggplotly ----
  if (interactive) {
    plot <- plotly::ggplotly(plot)
  }


  return(plot)

}
