#' Draw Interaction Plot
#'
#' @description
#' Draws an Interaction Plot
#'
#' @param data input dataset to be plotted (required)
#' @param y_var Y variable to be plotted on Y axis (required)
#' @param x_var_1_levels First grouping variable levels, e.g. -1/1 or "low"/"high" (required)
#' @param x_var_2_levels Second grouping variable levels, e.g. -1/1 or "low"/"high" (required)
#' @param alpha Set transparency. By default, it is set to 0.5  (optional)
#' @param analysis_desc_label analysis_desc_label Label (subtitle) for analysis description. By default, it is set to NULL  (optional)
#'
#' @return A ggplot Interaction Plot object
#'
#' @export

draw_interaction_plot <- function(data, y_var, x_var_1_levels, x_var_2_levels, alpha = 0.5, analysis_desc_label = NULL) {

  # 1. TIDY EVAL
  y_var_expr          <- rlang::enquo(y_var)
  x_var_1_expr        <- rlang::enquo(x_var_1_levels)
  x_var_2_expr        <- rlang::enquo(x_var_2_levels)

  # 2. DATA TRANSFORMATION
  data <- data %>%
    dplyr::mutate(!!x_var_1_expr := !!x_var_1_expr %>% forcats::as_factor()) %>%
    dplyr::mutate(!!x_var_2_expr := !!x_var_2_expr %>% forcats::as_factor())

  # 3. PLOTTING FUNCTION
  plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = !!x_var_1_expr, y = !!y_var_expr, color = !!x_var_2_expr)) +
    ggplot2::geom_point(alpha = alpha, size = 4, shape = 21, stroke = 1.2) +
    ggplot2::stat_summary(ggplot2::aes(y = !!y_var_expr, group = !!x_var_2_expr), fun = mean, size = 1, geom = "line", alpha = alpha) +
    sherlock::theme_sherlock() +
    sherlock::scale_color_sherlock() +
    ggplot2::labs(title = "Interaction Plot", subtitle = analysis_desc_label)

  return(plot)

}
