#' Draw Grouped Pareto Chart
#'
#' @description
#' Draws a small multiples type of Pareto Chart grouped by a categorical variable
#'
#' @param data input dataset to be plotted (required)
#' @param cat_var Categorical variable (required)
#' @param grouping_var Grouping variable (required)
#' @param summarize  Logical. If FALSE, default, the function expects total counts of each category of the categorical variable. If TRUE, individual values within each category are automatically summed up and ranked. (required)
#' @param continuous_var  Continuous variable to rank by (e.g. sum, frequency etc.). Not required if summarize argument is set to TRUE. (required)
#' @param drop_na  Logical. If TRUE, default, NA values of the categorical variable are dropped. (required)
#' @param highlight_first_n_items  Specify the top n items to be highlighted. By default, it is set to 0. (optional)
#' @param lump_last_n_items  Specify the last n items to be lumped into one category. By default, it is set to 0. (optional)
#' @param lumped_cat_name  Name lumped category. By default, it is set to "Other". (optional)
#' @param color  Set panel fill color for facets. Options are "one" (one color) or "multi" (each panel is a different color). By default, it is set to "one". (optional)
#' @param scale Specify an acceptable argument for scale. Acceptable arguments are "numeric", "percent", "dollar", "dollar-k" or "dollar-M". By default, it is set to "numeric" (optional)
#' @param accuracy Number to round to. Default value is set to 1. If NULL, values will be rounded to the nearest integer. (optional)
#' @param title_label  Specify plot title. By default, it is set to display "Pareto Chart" (optional)
#' @param analysis_desc_label  Specify plot analysis desc label (subtitle). By default, it is set to display CONTINUOUS VARIABLE COLUMN NAME "by" CATEGORICAL VARIABLE COLUMN NAME (optional)
#' @param axis_text_size Set axis text size. By default, it is set at 10. (optional)
#' @param x_axis_span Set X axis span. Options are "free" (a different span for each panel based on range of values for each panel) and "fixed" (the X axes in all panels are set to span the total range of all values). By default, it is set to "free". (optional)
#'
#' @return A 'ggplot' object
#'
#' @export

draw_pareto_chart_grouped <- function(data, cat_var, grouping_var, summarize = FALSE, continuous_var, drop_na = TRUE,
                                      highlight_first_n_items = 0, lump_last_n_items = 0, lumped_cat_name = "Other",
                                      color = "one", scale = "numeric", accuracy = 1,
                                      title_label = "Pareto Chart", analysis_desc_label = NULL, axis_text_size = 10,
                                      x_axis_span = "free") {


  # 1. WARNINGS ----
  if (!(scale %in% c("numeric", "percent", "dollar", "dollar-k", "dollar-M"))) {
    warning('Specify an acceptable argument for scale. Acceptable arguments are "numeric", "percent", "dollar", "dollar-k" or "dollar-M"')
  }

  if (!summarize && missing(continuous_var)) {
    warning('Since the summarize argument has been set to FALSE, you must select a continuous variable.')
  }

  if (!(color %in% c("one", "multi"))) {
    warning('Specify an acceptable argument for scale. Acceptable arguments are "one" or "multi".')
  }

  if (!(x_axis_span %in% c("free", "fixed"))) {
    warning('Specify an acceptable argument for x_axis_span. Acceptable arguments are "free" or "fixed".')
  }

  # 2. TIDY EVAL ----
  cat_var_expr   <- rlang::enquo(cat_var)
  continuous_var_expr <- rlang::enquo(continuous_var)
  grouping_var_expr <- rlang::enquo(grouping_var)


  # 3.1 BOOLEAN - SUMMARIZE INDIVIDUAL VALUES ----
  if (summarize) {

    if (drop_na) {
      data <- data %>%
        tidyr::drop_na(!!cat_var_expr)
    }

    data <- data %>%
      dplyr::count(!!grouping_var_expr, !!cat_var_expr) %>%
      dplyr::mutate(!!cat_var_expr := tidytext::reorder_within(!!cat_var_expr, n, !!grouping_var_expr)) %>%
      dplyr::group_by(!!grouping_var_expr) %>%
      dplyr::arrange(dplyr::desc(n), .by_group = TRUE) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::as_factor() %>% forcats::fct_reorder(n)) %>%
      dplyr::ungroup()

    data <- data %>%
      dplyr::mutate(alpha = dplyr::case_when(rank <= highlight_first_n_items ~ 0.7,
                                             TRUE ~ 0.3))


    if (lump_last_n_items > 0) {
      data <- data %>%
        dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::fct_lump(n = (data %>% dplyr::pull(!!(cat_var_expr)) %>% length()) - lump_last_n_items,
                                                                                 w = n,
                                                                                 other_level = lumped_cat_name)) %>%
        dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::fct_reorder(n)) %>%
        dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::fct_relevel(lumped_cat_name, after = 0))
    }

    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(!!cat_var_expr, n, fill = !!grouping_var_expr))

    if (color == "one") {
      plot <- plot +
        ggplot2::geom_col(fill = sherlock::scale_fill_sherlock(3), alpha = data$alpha)
    }

    if (color == "multi") {
      plot <- plot +
        ggplot2::geom_col(ggplot2::aes(fill = !!grouping_var_expr), alpha = data$alpha)
    }


    if (x_axis_span == "fixed") {
      plot <- plot +
        ggplot2::facet_wrap(facets = ggplot2::vars(!!grouping_var_expr), scales = "free_y")
    }

    if (x_axis_span == "free") {
      plot <- plot +
        ggplot2::facet_wrap(facets = ggplot2::vars(!!grouping_var_expr), scales = "free")
    }


    plot <- plot +
      ggplot2::coord_flip() +
      tidytext::scale_x_reordered() +
      sherlock::scale_fill_sherlock() +
      sherlock::theme_sherlock() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     plot.title   = ggplot2::element_text(size = 18),
                     axis.text    = ggplot2::element_text(size = axis_text_size),
                     axis.title.y = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::labs(title    = title_label,
                    subtitle = ifelse(is.null(analysis_desc_label),
                                      stringr::str_glue("{as_label(cat_var_expr)} by {as_label(grouping_var_expr)}"),
                                      analysis_desc_label))
  }

  # 3.2 BOOLEAN - NOT SUMMARIZE INDIVIDUAL VALUES ----
  if (!summarize) {

    if (drop_na) {
      data <- data %>%
        tidyr::drop_na(!!cat_var_expr)
    }

    data <- data %>%
      dplyr::group_by(!!grouping_var_expr) %>%
      dplyr::arrange(dplyr::desc(!!continuous_var_expr), .by_group = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!cat_var_expr := tidytext::reorder_within(!!cat_var_expr, !!continuous_var_expr, !!grouping_var_expr)) %>%
      dplyr::group_by(!!grouping_var_expr) %>%
      dplyr::arrange(dplyr::desc(!!continuous_var_expr), .by_group = TRUE) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::as_factor() %>% forcats::fct_reorder(!!continuous_var_expr)) %>%
      dplyr::ungroup()

    data <- data %>%
      dplyr::mutate(alpha = dplyr::case_when(rank <= highlight_first_n_items ~ 0.7,
                                             TRUE ~ 0.3))


    if (lump_last_n_items > 0) {
      data <- data %>%
        dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::fct_lump(n = (data %>% dplyr::pull(!!(cat_var_expr)) %>% length()) - lump_last_n_items,
                                                                                 w = !!continuous_var_expr,
                                                                                 other_level = lumped_cat_name)) %>%
        dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::fct_reorder(!!continuous_var_expr)) %>%
        dplyr::mutate(!!(cat_var_expr) := !!(cat_var_expr) %>% forcats::fct_relevel(lumped_cat_name, after = 0))
    }

    plot <- data %>%
      ggplot2::ggplot(ggplot2::aes(!!cat_var_expr, !!continuous_var_expr, fill = !!grouping_var_expr))

    if (color == "one") {
      plot <- plot +
        ggplot2::geom_col(fill = sherlock::scale_fill_sherlock(3), alpha = data$alpha)
    }

    if (color == "multi") {
      plot <- plot +
        ggplot2::geom_col(ggplot2::aes(fill = !!grouping_var_expr), alpha = data$alpha)
    }


    if (x_axis_span == "fixed") {
      plot <- plot +
        ggplot2::facet_wrap(facets = ggplot2::vars(!!grouping_var_expr), scales = "free_y")
    }

    if (x_axis_span == "free") {
      plot <- plot +
        ggplot2::facet_wrap(facets = ggplot2::vars(!!grouping_var_expr), scales = "free")
    }


    plot <- plot +
      ggplot2::coord_flip() +
      tidytext::scale_x_reordered() +
      sherlock::scale_fill_sherlock() +
      sherlock::theme_sherlock() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     plot.title   = ggplot2::element_text(size = 18),
                     axis.text    = ggplot2::element_text(size = axis_text_size),
                     axis.title.y = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::labs(title    = title_label,
                    subtitle = ifelse(is.null(analysis_desc_label),
                                      stringr::str_glue("{as_label(cat_var_expr)} by {as_label(grouping_var_expr)}"),
                                      analysis_desc_label))

  }



  # 4. SCALES AND ACCURACY ----
  if (scale == "numeric") {
    plot <- plot + ggplot2::scale_y_continuous(position = "right", labels = scales::number_format(accuracy = accuracy))
  }

  if (scale == "percent") {
    plot <- plot + ggplot2::scale_y_continuous(position = "right", labels = scales::percent_format(accuracy = accuracy))
  }

  if (scale == "dollar") {
    plot <- plot + ggplot2::scale_y_continuous(position = "right", labels = scales::dollar_format(accuracy = accuracy))
  }

  if (scale == "dollar-k") {
    plot <- plot + ggplot2::scale_y_continuous(position = "right", labels = scales::dollar_format(scale = 0.001, suffix = "k", accuracy = accuracy))
  }

  if (scale == "dollar-M") {
    plot <- plot + ggplot2::scale_y_continuous(position = "right", labels = scales::dollar_format(scale = 0.000001, suffix = "M", accuracy = accuracy))
  }


  return(plot)

}
