#' Draw Small Multiples
#'
#' @description
#' Draws a Small Multiples Plot
#'
#' @param data input dataset to be plotted (required)
#' @param x_axis_var variable to be plotted on x axis (required)
#' @param y_axis_var variable to be plotted on x axis (required)
#' @param grouping_var set grouping variable (required)
#' @param lowest_highest_units takes a vector of strings corresponding to the lowest/ihghest units to be highlighted (optional)
#' @param faceting set whether to display each group in a separate plot. By default, it is set to TRUE (optional)
#' @param unique_color_by_group set whether to display each group in a unique color. By default, it is set to TRUE (optional)
#' @param interactive set plot interactivity. By default, it is set to FALSE (optional)
#'
#' @return A ggplot Small Multiples object
#'
#' @export


draw_small_multiples <- function(data, x_axis_var, y_axis_var, grouping_var, lowest_highest_units, faceting = TRUE, unique_color_by_group = TRUE,
                                 interactive = FALSE) {

  x_axis_var_expr <- rlang::enquo(x_axis_var)
  y_axis_var_expr <- rlang::enquo(y_axis_var)
  grouping_var_expr <- rlang::enquo(grouping_var)

  data <- data %>%
    dplyr::mutate(!!grouping_var_expr := forcats::as_factor(!!grouping_var_expr))


  if (!missing(lowest_highest_units)) {
    data <- data %>%
      dplyr::mutate(color = dplyr::case_when(!!grouping_var_expr %in% lowest_highest_units ~ "darkblue",
                               TRUE ~ "grey60")) %>%
      dplyr::mutate(size = dplyr::case_when(!!grouping_var_expr %in% lowest_highest_units ~ 0.7,
                              TRUE ~ 0.5))
  }


  # Plotting
  if (!missing(lowest_highest_units)) {

    if (faceting && unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(ggplot2::aes(color = !!grouping_var_expr), alpha = 0.4, size = 0.7) +
        ggplot2::facet_wrap(ggplot2::vars(!!grouping_var_expr))
    }

    if (faceting && !unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(color = "grey60", alpha = 0.4, size = 0.7) +
        ggplot2::facet_wrap(ggplot2::vars(!!grouping_var_expr))
    }

    if (!faceting && unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(ggplot2::aes(color = !!grouping_var_expr), alpha = 0.4, size = 0.7)
    }

    if (!faceting && !unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(color = "grey60", alpha = 0.4, size = 0.7)
    }

  } else {

    if (faceting && unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(ggplot2::aes(color = !!grouping_var_expr), alpha = 0.4, size = data$size) +
        ggplot2::facet_wrap(ggplot2::vars(!!grouping_var_expr))
    }

    if (faceting && !unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(color = data$color, alpha = 0.4, size = data$size) +
        ggplot2::facet_wrap(ggplot2::vars(!!grouping_var_expr))
    }

    if (!faceting && unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(ggplot2::aes(color = !!grouping_var_expr), alpha = 0.4, size = data$size)
    }

    if (!faceting && !unique_color_by_group) {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_axis_var_expr, !!y_axis_var_expr, group = !!grouping_var_expr)) +
        ggplot2::geom_line(color = data$color, alpha = 0.4, size = data$size)

    }

  }





  # Plot theme ----
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
      plot.title       = ggplot2::element_text(hjust = 0, size = 16, color = "grey50")) +
    ggplot2::labs(
      title = "Small Multiples Plot"
    ) +
  sherlock::scale_color_sherlock()


  if (interactive) {
    plot <- plotly::ggplotly(plot)
  }


  return(plot)

}
