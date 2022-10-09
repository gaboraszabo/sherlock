#' Draw Interaction Plot
#'
#' @description
#' Draws an Interaction Plot
#'
#' @param data input dataset to be plotted (required)
#' @param y_var Y variable to be plotted on Y axis (required)
#' @param x_var_1 First grouping variable (optional)
#' @param x_var_2 Second, higher-level grouping variable  (optional)
#' @param alpha Set transparency. By default, it is set to 0.5  (optional)
#' @param analysis_desc Set whether to add jitter. By default, it is set to TRUE  (optional)
#'
#' @return A ggplot Interaction Plot object
#'
#' @export

draw_interaction_plot <- function(data, y_var, x_var_1, x_var_2, alpha = 0.5, analysis_desc) {

  # 1. TIDY EVAL
  y_var_expr          <- rlang::enquo(y_var)
  x_var_1_expr        <- rlang::enquo(x_var_1)
  x_var_2_expr        <- rlang::enquo(x_var_2)

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
    ggplot2::labs(title = "Interaction Plot", subtitle = analysis_desc)

  return(plot)

}
