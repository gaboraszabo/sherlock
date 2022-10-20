#' Draw Pareto Chart
#'
#' @description
#' Draws a Pareto Chart
#'
#' @param data input dataset to be plotted (required)
#' @param cat_var categorical variable, Y (required)
#' @param y_var  (required)
#' @param highlight_first_n_items  (required)
#' @param lump_last_n_items  (optional)
#' @param lumped_cat_name  (optional)
#' @param column_fill  (optional)
#' @param scale  (optional)
#' @param alpha  (optional)
#' @param axis_text_size (optional)
#'
#' @return A Pareto Chart ggplot object
#'
#' @export

draw_pareto_chart <- function(data, cat_var, y_var, highlight_first_n_items = 3, lump_last_n_items = 0,
                              lumped_cat_name = "Other", column_fill = scale_fill_sherlock(3), scale = "numeric", axis_text_size = 10) {


  cat_var_expr   <- rlang::enquo(cat_var)
  y_var_expr <- rlang::enquo(y_var)

  data <- data %>%
    arrange(desc(!!y_var_expr)) %>%
    mutate(rank = row_number()) %>%
    mutate(!!(cat_var_expr)   := !!(cat_var_expr) %>% as_factor() %>% fct_rev()) %>%
    mutate(!!(y_var_expr) := !!(y_var_expr) %>% as.numeric()) %>%
    mutate(fill = column_fill) %>%
    mutate(alpha = case_when(rank <= highlight_first_n_items ~ 0.8,
                             TRUE ~ 0.4))

  if (lump_last_n_items > 0) {
    data <- data %>%
      mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% fct_lump(n = (data %>% pull(!!(cat_var_expr)) %>% length()) - lump_last_n_items,
                                                               w = !!(y_var_expr),
                                                               other_level = lumped_cat_name)) %>%
      mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% fct_reorder(!!(y_var_expr))) %>%
      mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% fct_relevel(lumped_cat_name, after = 0))
  }

  plot <- data %>%
    ggplot(aes(!!(cat_var_expr), !!(y_var_expr))) +
    geom_col(fill = column_fill, alpha = data$alpha) +
    coord_flip() +
    theme_sherlock() +
    theme(axis.title.x = element_blank(),
          plot.title   = element_text(size = 18),
          axis.text.x  = element_text(size = axis_text_size)) +
    scale_y_continuous(position = "right") +
    labs(title    = "Pareto Chart",
         subtitle = str_glue("{as_label(y_var_expr)} by {as_label(cat_var_expr)}"))

  if (scale == "percent") {
    plot <- plot + scale_y_continuous(position = "right", labels = scales::percent_format(accuracy = 0.1))
  }

  if (scale == "dollars") {
    plot <- plot + scale_y_continuous(position = "right", labels = scales::dollar_format())
  }

  if (scale == "dollars-k") {
    plot <- plot + scale_y_continuous(position = "right", labels = scales::dollar_format(scale = 0.001, suffix = "k", accuracy = 1))
  }

  if (scale == "dollars-M") {
    plot <- plot + scale_y_continuous(position = "right", labels = scales::dollar_format(scale = 0.000001, suffix = "M", accuracy = 0.1))
  }


  return(plot)

}
