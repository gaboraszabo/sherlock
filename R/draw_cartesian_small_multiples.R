#' Draw Cartesian Small Multiples Plot
#'
#' @description
#' Draws a cartesian small multiples plot
#'
#' @param data Input dataset to be plotted (required)
#' @param x_coord Column for X coordinate values (required)
#' @param y_coord Column for Y coordinate values (required)
#' @param grouping_var Grouping variable. Each group is displayed in a different color. (optional)
#' @param faceting_var_1 Set first faceting variable (optional)
#' @param faceting_var_2 Set second faceting variable (optional)
#' @param interactive Set plot interactivity. By default, it is set to FALSE (optional)
#' @param size Set point size. By default, it is set to 2  (optional)
#' @param alpha Set transparency. By default, it is set to 0.4  (optional)
#' @param analysis_desc_label Label (subtitle) for analysis description. By default, it is set to NULL  (optional)
#' @param x_axis_label Label for x axis. By default, it is set to display x axis column name  (optional)
#' @param y_axis_label Label for y axis. By default, it is set to display y axis column name  (optional)
#' @param n_breaks_x_axis Set number of breaks on X axis. By default, it is set to 10 (optional)
#' @param n_breaks_y_axis Set number of breaks on Y axis. By default, it is set to 10 (optional)
#' @param accuracy Set number of decimal places to be displayed on X and Y axes. Examples: 0.1 - one decimal place, 0.01 - two decimal places, 0.001 - three decimal places etc. By default, it is set to 0.001 (optional)
#' @param show_axis_values Logical. if FALSE, default, axis values are not shown (optional)
#'
#' @return A 'ggplot' or 'plotly' object
#'
#' @export


draw_cartesian_small_multiples <- function(data, x_coord, y_coord,
                                           grouping_var, faceting_var_1, faceting_var_2, interactive = FALSE,
                                           size = 2, alpha = 0.4,
                                           analysis_desc_label = NULL, x_axis_label = NULL, y_axis_label = NULL,
                                           n_breaks_x_axis = 10, n_breaks_y_axis = 10, accuracy = 0.001, show_axis_values = TRUE) {


  # 1. Tidy Eval ----
  x_expr <- rlang::enquo(x_coord)
  y_expr <- rlang::enquo(y_coord)
  grouping_var_expr <- rlang::enquo(grouping_var)
  faceting_var_1_expr <- rlang::enquo(faceting_var_1)
  faceting_var_2_expr <- rlang::enquo(faceting_var_2)



  # 2. Convert grouping variable and faceting variables to factor ----
  if (!missing(grouping_var)) {
    data <- data %>%
      dplyr::mutate(!!grouping_var_expr := forcats::as_factor(!!grouping_var_expr))
  }

  if (!missing(faceting_var_1) & missing(faceting_var_2)) {
    data <- data %>%
      dplyr::mutate(!!faceting_var_1_expr := forcats::as_factor(!!faceting_var_1_expr))
  }


  if (!missing(faceting_var_1) & !missing(faceting_var_2)) {
    data <- data %>%
      dplyr::mutate(!!faceting_var_1_expr := forcats::as_factor(!!faceting_var_1_expr),
                    !!faceting_var_2_expr := forcats::as_factor(!!faceting_var_2_expr))
  }




  # 3. Plot ----
  if (missing(grouping_var)) {
    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(!!x_expr, !!y_expr)) +
      ggplot2::geom_point(size = size, alpha = alpha, color = scale_color_sherlock(2))
  } else {
    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(!!x_expr, !!y_expr, color = !!grouping_var_expr)) +
      ggplot2::geom_point(size = size, alpha = alpha)
  }


  plot <- plot +
    ggplot2::coord_fixed(ratio = 1)




  # 4. Theme ----
  plot <- plot +
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(color = "grey70", face = "bold", size = 11),
      panel.border     = ggplot2::element_blank(),
      panel.spacing    = ggplot2::unit(1, "lines"),
      axis.text        = ggplot2::element_blank(),
      axis.title       = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      legend.title     = ggplot2::element_text(color = "grey50", size = 11),
      legend.text      = ggplot2::element_text(color = "grey50", size = 11),
      plot.title       = ggplot2::element_text(size = 16, color = "grey50"),
      plot.subtitle    = ggplot2::element_text(size = 10, color = "grey50")) +
    sherlock::scale_color_sherlock()


  # 5. Title ----
  if (interactive) {
    if (is.null(analysis_desc_label)) {
      plot <- plot +
        ggplot2::labs(
          title = "Cartesian Small Multiples Plot"
        )
    } else {
      plot <- plot +
        ggplot2::labs(
          title = stringr::str_glue("Cartesian Small Multiples Plot - {analysis_desc_label}")
        ) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 12, color = "grey50"))
    }

  } else {
    plot <- plot +
      ggplot2::labs(
        title = "Cartesian Small Multiples Plot",
        subtitle = analysis_desc_label
      )
  }



  plot <- plot +
    ggplot2::labs(
      x = ifelse(is.null(x_axis_label), stringr::str_glue("{as_label(x_expr)}"), x_axis_label),
      y = ifelse(is.null(y_axis_label), stringr::str_glue("{as_label(y_expr)}"), y_axis_label)
    )



  plot <- plot +
    ggplot2::scale_x_continuous(n.breaks = n_breaks_x_axis, labels = scales::number_format(accuracy = accuracy)) +
    ggplot2::scale_y_continuous(n.breaks = n_breaks_y_axis, labels = scales::number_format(accuracy = accuracy))


  # 6. Conditionals ----
  if (!missing(faceting_var_1) & missing(faceting_var_2)) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!faceting_var_1_expr))
  }


  if (!missing(faceting_var_1) & !missing(faceting_var_2)) {
    plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(!!faceting_var_1_expr),
                                       cols = ggplot2::vars(!!faceting_var_2_expr))
  }



  if (show_axis_values) {
    plot <- plot + ggplot2::theme(axis.text = ggplot2::element_text(size = 8, color = "grey70"))
  }


  # 8. Interactivity ----
  if (interactive) {
    plot <- plotly::ggplotly(plot)
  }


  return(plot)

}
