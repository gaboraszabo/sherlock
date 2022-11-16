#' Draw Cartesian Small Multiple Plot
#'
#' @description
#' Draws a cartesian small multiples plot
#'
#' @param data Input dataset to be plotted (required)
#' @param x_coord x coordinate values (required)
#' @param y_coord y coordinate values (required)
#' @param grouping_var_1 Grouping variable 1 (required)
#' @param grouping_var_2 Grouping variable 2 (optional)
#' @param four_quadrants Logical. Set whether to display four quadrant with both axes starting at zero. By default, it is set to FALSE (optional)
#' @param show_axis_values Logical. if FALSE, default, axis values are not shown (optional)
#' @param faceted Logical. if TRUE, default, plot will be faceted. Note: Cartesian plot is always faceted when there are two grouping variables. Drop grouping variable 2 for no faceting. (optional)
#' @param size Set point size. By default, it is set to 2  (optional)
#' @param alpha Set transparency. By default, it is set to 0.4  (optional)
#' @param analysis_desc_label Label (subtitle) for analysis description. By default, it is set to NULL  (optional)
#' @param interactive Set plot interactivity. By default, it is set to FALSE (optional)
#'
#' @return A 'ggplot' or 'plotly' object
#'
#' @export


draw_cartesian_small_multiples <- function(data, x_coord, y_coord, grouping_var_1, grouping_var_2, four_quadrants = FALSE,
                                           show_axis_values = FALSE, faceted = TRUE, size = 2, alpha = 0.4, analysis_desc_label = NULL,
                                           interactive = FALSE) {


  if (missing(grouping_var_1)) {
    stop("You must select at least one grouping variable.")
  }

  # 1. Tidy Eval ----
  x_expr <- rlang::enquo(x_coord)
  y_expr <- rlang::enquo(y_coord)
  grouping_var_1_expr <- rlang::enquo(grouping_var_1)
  grouping_var_2_expr <- rlang::enquo(grouping_var_2)
  grouping_var_1_2_expr <- rlang::enquos(grouping_var_1, grouping_var_2)

  # 2. Calculate Limits ----
  range_tbl <- data %>%
    dplyr::summarize(range_max_2 = range(!!x_expr, !!y_expr)[2],
                     range_min_2 = range(!!x_expr, !!y_expr)[1])

  min <- range_tbl %>%
    dplyr::pull(range_min_2)

  max <- range_tbl %>%
    dplyr::pull(range_max_2)

  range_vector <- abs(max - min)
  limit_scalar <- abs(range_vector / 20)

  abs_max <- max(abs(min), abs(max))

  x_and_y_limits <- c(-abs_max - limit_scalar, abs_max + limit_scalar)


  # 3. Convert grouping variables to factor ----
  if (missing(grouping_var_2)) {
    data <- data %>%
      dplyr::mutate(!!grouping_var_1_expr := forcats::as_factor(!!grouping_var_1_expr))
  } else {
    data <- data %>%
      dplyr::mutate(!!grouping_var_1_expr := forcats::as_factor(!!grouping_var_1_expr)) %>%
      dplyr::mutate(!!grouping_var_2_expr := forcats::as_factor(!!grouping_var_2_expr))
  }


  # 4. Plot ----
  plot <- data %>%

    ggplot2::ggplot(ggplot2::aes(!!x_expr, !!y_expr, color = !!grouping_var_1_expr))

  # 4.1 four_quadrants arg ----
  if (four_quadrants) {
    plot <- plot +
      ggplot2::geom_hline(yintercept = 0, color = "grey70") +
      ggplot2::geom_vline(xintercept = 0, color = "grey70")
  }

  plot <- plot +
    ggplot2::geom_point(size = size, alpha = alpha)

  # 4.2 four_quadrants arg ----
  if (four_quadrants) {
    plot <- plot +
      ggplot2::coord_fixed(
        ratio = 1,
        xlim = x_and_y_limits,
        ylim = x_and_y_limits)
  }

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
      plot.title    = ggplot2::element_text(hjust = 0.5, size = 16, color = "grey50")) +
    ggplot2::labs(
      title = "Cartesian Small Multiples Plot",
      subtitle = analysis_desc_label
    ) +
    sherlock::scale_color_sherlock()


  # 5. Conditionals ----
  if (missing(grouping_var_2)) {

    if (faceted) {
      plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!grouping_var_1_expr))
    } else {
      plot
    }

  } else {
    if (faceted) {
      plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(!!grouping_var_2_expr),
                                         cols = ggplot2::vars(!!grouping_var_1_expr))
    } else {
      message("Cartesian plot is always faceted when there are two grouping variables.\nDrop grouping variable 2 for no faceting.")

      plot <- plot + ggplot2::facet_grid(rows = ggplot2::vars(!!grouping_var_2_expr),
                                         cols = ggplot2::vars(!!grouping_var_1_expr))
    }

  }


  if (show_axis_values) {
    plot <- plot + ggplot2::theme(axis.text = ggplot2::element_text(size = 8, color = "grey70"))
  }


  # 6. Interactivity ----
  if (interactive) {
    plot <- plotly::ggplotly(plot)
  }


  return(plot)

}
