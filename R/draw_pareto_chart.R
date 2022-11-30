#' Draw Pareto Chart
#'
#' @description
#' Draws a Pareto Chart
#'
#' @param data input dataset to be plotted (required)
#' @param cat_var Categorical variable (required)
#' @param continuous_var  Continuous variable to rank by (e.g. sum, frequency etc.)  (required)
#' @param highlight_first_n_items  Specify the top n items to be highlighted. By default, it is set to 0. (optional)
#' @param lump_last_n_items  Specify the last n items to be lumped into one category. By default, it is set to 0. (optional)
#' @param lumped_cat_name  Name lumped category. By default, it is set to "Other". (optional)
#' @param column_fill  Column fill color. By default, it is set to scale_fill_sherlock(3) (optional)
#' @param scale Specify an acceptable argument for scale. Acceptable arguments are "numeric", "percent", "dollar", "dollar-k" or "dollar-M". By default, it is set to "numeric" (optional)
#' @param title_label  Specify plot title. By default, it is set to display "Pareto Chart" (optional)
#' @param analysis_desc_label  Specify plot analysis desc label (subtitle). By default, it is set to display CONTINUOUS VARIABLE COLUMN NAME "by" CATEGORICAL VARIABLE COLUMN NAME (optional)
#' @param axis_text_size Set axis text size. By default, it is set at 10. (optional)
#'
#' @return A 'ggplot' object
#'
#' @export

draw_pareto_chart <- function(data, cat_var, continuous_var, highlight_first_n_items = 0, lump_last_n_items = 0,
                              lumped_cat_name = "Other", column_fill = scale_fill_sherlock(3), scale = "numeric",
                              title_label = "Pareto Chart", analysis_desc_label = NULL, axis_text_size = 10) {


  if (!(scale %in% c("numeric", "percent", "dollar", "dollar-k", "dollar-M"))) {
    warning('Specify an acceptable argument for scale. Acceptable arguments are "numeric", "percent", "dollar", "dollar-k" or "dollar-M"')
  }


  cat_var_expr   <- rlang::enquo(cat_var)
  continuous_var_expr <- rlang::enquo(continuous_var)

  data <- data %>%
    dplyr::arrange(dplyr::desc(!!continuous_var_expr)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::as_factor() %>% forcats::fct_reorder(!!continuous_var_expr)) %>%
    dplyr::mutate(!!(continuous_var_expr) := !!(continuous_var_expr) %>% as.numeric()) %>%
    dplyr::mutate(fill = column_fill) %>%
    dplyr::mutate(alpha = dplyr::case_when(rank <= highlight_first_n_items ~ 0.8,
                                           TRUE ~ 0.4))

  if (lump_last_n_items > 0) {
    data <- data %>%
      dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::fct_lump(n = (data %>% dplyr::pull(!!(cat_var_expr)) %>% length()) - lump_last_n_items,
                                                                               w = !!(continuous_var_expr),
                                                                               other_level = lumped_cat_name)) %>%
      dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::fct_reorder(!!(continuous_var_expr))) %>%
      dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::fct_relevel(lumped_cat_name, after = 0))
  }

  plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(!!(cat_var_expr), !!(continuous_var_expr))) +
    ggplot2::geom_col(fill = column_fill, alpha = data$alpha) +
    ggplot2::coord_flip() +
    sherlock::theme_sherlock() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   plot.title   = ggplot2::element_text(size = 18),
                   axis.text.x  = ggplot2::element_text(size = axis_text_size),
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(position = "right") +
    ggplot2::labs(title    = title_label,
                  subtitle = ifelse(is.null(analysis_desc_label),
                                    stringr::str_glue("{as_label(continuous_var_expr)} by {as_label(cat_var_expr)}"),
                                    analysis_desc_label))

  if (scale == "percent") {
    plot <- plot + ggplot2::scale_y_continuous(position = "right", labels = scales::percent_format(accuracy = 0.1))
  }

  if (scale == "dollar") {
    plot <- plot + ggplot2::scale_y_continuous(position = "right", labels = scales::dollar_format())
  }

  if (scale == "dollar-k") {
    plot <- plot + ggplot2::scale_y_continuous(position = "right", labels = scales::dollar_format(scale = 0.001, suffix = "k", accuracy = 1))
  }

  if (scale == "dollar-M") {
    plot <- plot + ggplot2::scale_y_continuous(position = "right", labels = scales::dollar_format(scale = 0.000001, suffix = "M", accuracy = 0.1))
  }


  return(plot)

}
