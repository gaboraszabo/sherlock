#' Plot Tukey-Duckworth Paired Test
#'
#' @description
#' Plots Tukey-Duckworth Paired Test
#'
#' @param data input dataset (required)
#' @param y_var Y variable of interest (required)
#' @param x_vars X variables of interest (required)
#' @param arrows  Set whether to display arrows in the plot. By default, it is set to FALSE (optional)
#'
#' @return A 'ggplot' object
#'
#' @export

plot_tukey_duckworth_paired_test <- function(data, y_var, x_vars, arrows = FALSE) {

  y_var_expr <- rlang::enquo(y_var)
  x_vars_expr <- rlang::enquos(x_vars)

  y_var_label <- rlang::as_label(y_var_expr)
  x_vars_label <- rlang::as_label(x_vars_expr)


  # Y VAR ----
  data_y_var <- data %>%
    tidyr::pivot_longer(cols = {{ y_var }}, names_to = "Variable", values_to = "Value") %>%
    dplyr::select(Pair, Variable, Value, color_label) %>%
    dplyr::mutate(Pair = Pair %>% forcats::as_factor()) %>%
    dplyr::mutate(Variable = stringr::str_replace(Variable,
                                                  pattern = y_var_label,
                                                  replacement = stringr::str_glue("Y - {y_var_label}"))) %>%
    dplyr::arrange(Pair)

  data_y_var_arrow_color <- data_y_var %>%
    tidyr::pivot_wider(names_from = "color_label", values_from = "Value") %>%
    dplyr::mutate(diff = black - red) %>%
    tidyr::pivot_longer(cols = 3:4, names_to = "color_label", values_to = "Value")


  plot_y_var <- data_y_var %>%
    ggplot2::ggplot(ggplot2::aes(Pair, Value))

  if (!arrows) {
    plot_y_var <- plot_y_var + ggplot2::geom_point(size   = 3.5,
                                                   color  = data_y_var$color_label,
                                                   fill   = data_y_var$color_label,
                                                   stroke = 0,
                                                   alpha  = 0.5)
  }

  if (arrows) {
    plot_y_var <- plot_y_var + ggplot2::geom_line(arrow = ggplot2::arrow(length = unit(0.25, "cm"),
                                                                         type   = "closed"),
                                                  ggplot2::aes(group = Pair),
                                                  color = dplyr::case_when(data_y_var_arrow_color$diff > 0  ~ "red",
                                                                           data_y_var_arrow_color$diff < 0  ~ "black",
                                                                           TRUE ~ "#3971CB"),
                                                  alpha  = 0.4)

  }

  plot_y_var <- plot_y_var +
    ggplot2::facet_wrap(ggplot2::vars(Variable), scales = "free_y") +
    sherlock::theme_sherlock() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 0, 5, "cm"),
                   plot.title  = ggplot2::element_text(hjust = 0.5, size = 16)) +
    ggplot2::labs(title = "Tukey-Duckworth Paired Test")



  # X VARS ----
  data_x_var <- data %>%
    tidyr::pivot_longer(cols = {{ x_vars }}, names_to = "Variable", values_to = "Value") %>%
    dplyr::select(Pair, Variable, Value, color_label) %>%
    dplyr::mutate(Pair = Pair %>% forcats::as_factor()) %>%
    dplyr::mutate(Variable = Variable %>% forcats::as_factor()) %>%
    dplyr::arrange(Variable, Pair)

  data_x_var_arrow_color <- data_x_var %>%
    tidyr::pivot_wider(names_from = "color_label", values_from = "Value") %>%
    dplyr::mutate(diff = black - red) %>%
    tidyr::pivot_longer(cols = 3:4, names_to = "color_label", values_to = "Value") %>%
    dplyr::mutate(Pair = Pair %>% forcats::as_factor()) %>%
    dplyr::mutate(Variable = Variable %>% forcats::as_factor()) %>%
    dplyr::arrange(Variable, Pair)

  data_x_var_arrow_color <- data_x_var_arrow_color %>%
    dplyr::mutate(color_label_arrow = dplyr::case_when(data_x_var_arrow_color$diff < 0  ~ "black",
                                                       data_x_var_arrow_color$diff > 0  ~ "red",
                                                       TRUE ~ "#3971CB"))


  data_x_var <- data_x_var %>%
    dplyr::left_join(data_x_var_arrow_color)


  plot_x_vars <- data_x_var %>%
    ggplot2::ggplot(ggplot2::aes(Pair, Value))

  if (!arrows) {
    plot_x_vars <- plot_x_vars + ggplot2::geom_point(size   = 3.5,
                                                     color  = data_x_var$color_label,
                                                     fill   = data_x_var$color_label,
                                                     stroke = 0,
                                                     alpha  = 0.5)
  }

  if (arrows) {
    plot_x_vars <- plot_x_vars + ggplot2::geom_line(arrow = ggplot2::arrow(length = unit(0.25, "cm"),
                                                                           type   = "closed"),
                                                    ggplot2::aes(group = Pair),
                                                    color  = data_x_var$color_label_arrow,
                                                    alpha  = 0.4)


  }

  plot_x_vars <- plot_x_vars +
    ggplot2::facet_wrap(ggplot2::vars(Variable), scales = "free_y") +
    sherlock::theme_sherlock()

  # COMBINING THE TWO PLOTS ----
  plot <- cowplot::plot_grid(plot_y_var,
                             plot_x_vars,
                             ncol        = 1,
                             rel_heights = c(1, 1))



  return(plot)
}
