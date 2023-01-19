#' Draw Process Behavior Chart
#'
#' @description
#' Draws a Process Behavior Chart
#'
#' @param data input dataset to be plotted (required)
#' @param y_var Y variable to be plotted on Y axis (required)
#' @param grouping_var Variable to group by (optional)
#' @param limits Logical. If TRUE, natural process limits (control limits) are plotted. By default, it is set to FALSE  (optional)
#' @param interactive Set plot interactivity. By default, it is set to TRUE (optional)
#'
#' @return A 'ggplot' or 'plotly' object
#'
#' @export

draw_process_behavior_chart <- function(data, y_var, grouping_var, limits = TRUE, interactive = TRUE) {

  # 1. Tidy Eval ----
  y_var_expr        <- rlang::enquo(y_var)
  grouping_var_expr <- rlang::enquo(grouping_var)


  if (limits) {

    if (missing(grouping_var)) {
      data <- data %>%
        dplyr::mutate(avg = mean(!!y_var_expr, na.rm = TRUE)) %>%
        dplyr::mutate(moving_range = abs(dplyr::lag(!!y_var_expr) - !!y_var_expr)) %>%
        dplyr::mutate(lcl = mean(avg) - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
        dplyr::mutate(ucl = mean(avg) + 2.66 * mean(moving_range, na.rm = TRUE)) %>%
        dplyr::mutate(index = 1:dplyr::n())
    } else {
      data <- data %>%
        dplyr::group_by(!!grouping_var_expr) %>%
        dplyr::mutate(avg = mean(!!y_var_expr, na.rm = TRUE)) %>%
        dplyr::mutate(moving_range = abs(dplyr::lag(!!y_var_expr) - !!y_var_expr)) %>%
        dplyr::mutate(lcl = mean(avg) - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
        dplyr::mutate(ucl = mean(avg) + 2.66 * mean(moving_range, na.rm = TRUE)) %>%
        dplyr::mutate(index = 1:dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = 1:dplyr::n())
    }

  } else {
    if (missing(grouping_var)) {
      data <- data %>%
        dplyr::mutate(avg = mean(!!y_var_expr, na.rm = TRUE)) %>%
        dplyr::mutate(index = 1:n())
    } else {
      data <- data %>%
        dplyr::group_by(!!grouping_var_expr) %>%
        dplyr::mutate(avg = mean(!!y_var_expr, na.rm = TRUE)) %>%
        dplyr::mutate(index = 1:n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = 1:n())
    }
  }


  # Plotting function ----
  if (missing(grouping_var)) {
    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(index, !!y_var_expr)) +
      ggplot2::geom_line(color = "#304269", size = 0.5, alpha = 0.4) +
      ggplot2::geom_point(color = "#304269", size = 1, alpha = 0.6)
  } else {
    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(index, !!y_var_expr)) +
      ggplot2::geom_line(color = "#304269", size = 0.5, alpha = 0.4) +
      ggplot2::geom_point(color = "#304269", size = 1, alpha = 0.6) +

      ggh4x::facet_nested(rows = ggplot2::vars(), cols = ggplot2::vars(!!grouping_var_expr),
                          scales = "free_x", independent = "x")
  }


  # Avg line ----
  if (missing(grouping_var)) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(index, avg), color = "grey60", size = 0.5, alpha = 0.7, linetype = "dashed")
  } else {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(n, avg), color = "grey60", size = 0.5, alpha = 0.7, linetype = "dashed")
  }



  # Control Limits ----
  if (limits) {

    if (missing(grouping_var)) {
      plot <- plot +
        ggplot2::geom_line(ggplot2::aes(index, lcl), color = "#700808", size = 0.5, alpha = 0.3) +
        ggplot2::geom_line(ggplot2::aes(index, ucl), color = "#700808", size = 0.5, alpha = 0.3)
    } else {
      plot <- plot +
        ggplot2::geom_line(ggplot2::aes(n, lcl), color = "#700808", size = 0.5, alpha = 0.3) +
        ggplot2::geom_line(ggplot2::aes(n, ucl), color = "#700808", size = 0.5, alpha = 0.3)

    }
  }




  # Plot theme ----
  plot <- plot +
    sherlock::theme_sherlock() +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(color = "grey50", face = "bold", size = 11),
      panel.border     = ggplot2::element_rect(colour = "grey70"),
      panel.spacing    = ggplot2::unit(0, "lines"),
      axis.title.x     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.title.y     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.text        = ggplot2::element_text(size = 11, color = "grey50"),
      legend.title     = ggplot2::element_text(color = "grey50", size = 11),
      legend.text      = ggplot2::element_text(color = "grey50", size = 11),
      plot.title       = ggplot2::element_text(hjust = 0, size = 16, color = "grey50")) +
    ggplot2::labs(
      title = "Process Behavior Chart",
      x = "Observations"
    )

  # Interactivity with plotly ----
  if (interactive) {
    plot <- plotly::ggplotly(plot)
  } else {
    plot
  }

  return(plot)

}
