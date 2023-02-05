#' Draw Small Multiples Line Plot
#'
#' @description
#' Draws a Small Multiples Line Plot
#'
#' @param data input dataset to be plotted (required)
#' @param x_axis_var variable to be plotted on x axis (required)
#' @param y_axis_var variable to be plotted on x axis (required)
#' @param grouping_var set grouping variable (required)
#' @param faceting_var_1 Set first faceting variable (optional)
#' @param faceting_var_2 Set second faceting variable (optional)
#' @param lowest_highest_units takes a vector of strings corresponding to the lowest/highest units to be highlighted (optional)
#' @param unique_color_by_group set whether to display each group in a unique color. By default, it is set to FALSE (optional)
#' @param size Set line size. By default, it is set to 0.7  (optional)
#' @param alpha Set transparency. By default, it is set to 0.4  (optional)
#' @param interactive set plot interactivity. By default, it is set to TRUE (optional)
#' @param analysis_desc_label Label (subtitle) for analysis description. By default, it is set to NULL  (optional)
#' @param x_axis_label Label for x axis. By default, it is set to display x axis column name  (optional)
#' @param y_axis_label Label for y axis. By default, it is set to display y axis column name  (optional)
#'
#' @return A 'ggplot' or 'plotly' object
#'
#' @export


draw_small_multiples_line_plot <- function(data, x_axis_var, y_axis_var, grouping_var, faceting_var_1, faceting_var_2,
                                           lowest_highest_units, unique_color_by_group = FALSE,
                                           size = 0.7, alpha = 0.4, interactive = TRUE,
                                           analysis_desc_label = NULL, x_axis_label = NULL, y_axis_label = NULL) {

  # 1. Tidy Eval ----
  x_axis_var_expr <- rlang::enquo(x_axis_var)
  y_axis_var_expr <- rlang::enquo(y_axis_var)
  grouping_var_expr <- rlang::enquo(grouping_var)
  faceting_var_1_expr <- rlang::enquo(faceting_var_1)
  faceting_var_2_expr <- rlang::enquo(faceting_var_2)

  data <- data %>%
    dplyr::mutate(!!grouping_var_expr := forcats::as_factor(!!grouping_var_expr))


  # 2. Color and size columns for lowest_highest_units arg ----
  if (!missing(lowest_highest_units)) {
    data <- data %>%
      dplyr::mutate(color = dplyr::case_when(!!grouping_var_expr %in% lowest_highest_units ~ "darkblue",
                                             TRUE ~ "grey60")) %>%
      dplyr::mutate(size = dplyr::case_when(!!grouping_var_expr %in% lowest_highest_units ~ 1.5*size,
                                            TRUE ~ size))
  }


  # 3. Plotting Function ----
  if (missing(lowest_highest_units)) {

    if (unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(ggplot2::aes(color = !!grouping_var_expr), alpha = alpha, size = size)
    }

    if (!unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(color = "grey60", alpha = alpha, size = size)
    }

  }


  if (!missing(lowest_highest_units)) {

    if (unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(ggplot2::aes(color = !!grouping_var_expr), alpha = alpha, size = data$size)
    }

    if (!unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(color = data$color, alpha = alpha, size = data$size)

    }

  }


  # 3.1 Faceting ----
  if (!missing(faceting_var_1) & missing(faceting_var_2)) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!faceting_var_1_expr))
  }


  if (!missing(faceting_var_1) & !missing(faceting_var_2)) {
    plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(!!faceting_var_1_expr),
                                       cols = ggplot2::vars(!!faceting_var_2_expr))
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




  # 6. Interactivity with ggplotly ----
  if (interactive) {
    plot <- plotly::ggplotly(plot)
  }


  return(plot)

}
