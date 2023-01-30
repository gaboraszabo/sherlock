#' Plot Tukey-Duckworth Test
#'
#' @description
#' Plots Tukey-Duckworth Paired Test
#'
#' @param data input dataset (required)
#' @param y_var Y variable of interest (required)
#' @param x_var_levels Levels of X variable of interest (required)
#' @param point_size Set point size. By default, it is set to 3. (optional)
#' @param point_type Set point size. Options are "solid" (default) and "no fill". (optional)
#' @param split_levels Set whether to plot the two levels in separately on the X axis. By default, it is set to FALSE (optional)
#' @param analysis_desc_label Label (subtitle) for analysis description. By default, it is set to NULL  (optional)
#'
#' @return A 'ggplot' object
#'
#' @export

plot_tukey_duckworth_test <- function(data, y_var, x_var_levels, point_size = 3, point_type = "solid",
                            split_levels = FALSE, analysis_desc_label = NULL) {

  y_var_expr <- rlang::enquo(y_var)
  x_var_levels_expr <- rlang::enquo(x_var_levels)


  data <- data %>%
    dplyr::mutate(variable = "variable")



  if (!split_levels) {
    if (point_type == "solid") {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(variable, !!y_var_expr, color = !!x_var_levels_expr)) +
        ggplot2::geom_jitter(width = 0.06, height = 0, alpha = 0.4, size = point_size) +
        sherlock::theme_sherlock() +
        ggplot2::scale_color_manual(values = c("red", "black"))

    }

    if (point_type == "no fill") {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(variable, !!y_var_expr, color = !!x_var_levels_expr)) +
        ggplot2::geom_jitter(width = 0.06, height = 0, alpha = 0.6, size = point_size, shape = 21, stroke = 1.5) +
        sherlock::theme_sherlock() +
        ggplot2::scale_color_manual(values = c("red", "black"))
    }

  }


  if (split_levels) {
    if (point_type == "solid") {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_var_levels_expr, !!y_var_expr, color = !!x_var_levels_expr)) +
        ggplot2::geom_jitter(width = 0.06, height = 0, alpha = 0.4, size = point_size) +
        sherlock::theme_sherlock() +
        ggplot2::scale_color_manual(values = c("red", "black"))

    }

    if (point_type == "no fill") {
      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!x_var_levels_expr, !!y_var_expr, color = !!x_var_levels_expr)) +
        ggplot2::geom_jitter(width = 0.06, height = 0, alpha = 0.6, size = point_size, shape = 21, stroke = 1.5) +
        sherlock::theme_sherlock() +
        ggplot2::scale_color_manual(values = c("red", "black"))
    }

  }





  if (!split_levels) {
    plot <- plot +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x  = ggplot2::element_blank())
  }

  if (split_levels) {
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }

  # LABELS ----
  plot <- plot +
    ggplot2::labs(title = "Tukey-Duckworth Test",
                  subtitle = analysis_desc_label)



  return(plot)

}

