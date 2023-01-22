#' Draw Timeseries Scatterplot
#'
#' @description
#' Draws a Timeseries Scatterplot
#'
#' @param data input dataset to be plotted (required)
#' @param y_var Y variable to be plotted on Y axis (required)
#' @param grouping_var_1 Time variable to be plotted on x axis (required)
#' @param grouping_var_1_type Time variable type. Options are "date-time" or "factor"
#' @param grouping_var_2 Additional variable for faceting (optional)
#' @param faceting Set whether to display each group in a separate plot. By default, it is set to FALSE (optional)
#' @param limits Logical. If TRUE, process behavior chart control limits for the individual group means are plotted. By default, it is set to FALSE  (optional)
#' @param date_breaks Set date breaks. Takes a string, for example "1 week" or "2 days". By default, it is set to "1 month" (optional)
#' @param date_labels Set date labels. Identical to the date labels argument of the scale_x_date() ggplot function (optional)
#' @param analysis_desc_label Label (subtitle) for analysis description. By default, it is set to NULL  (optional)
#' @param x_axis_text_size X axis text size. By default, it is set to 11. (optional)
#' @param point_size Set point size. By default, it is set to 1  (optional)
#' @param alpha Set transparency for individual observations. Identical to the alpha ggplot argument. By default, it is set to 0.3 (optional)
#' @param line_size Set line size. By default, it is set to 1  (optional)
#' @param interactive Set plot interactivity. By default, it is set to TRUE (optional)
#'
#' @return A 'ggplot' or 'plotly' object
#'
#' @examples
#' timeseries_scatterplot_data %>%
#'    draw_timeseries_scatterplot(y_var = y,
#'                                grouping_var_1 = date,
#'                                grouping_var_2 = cavity,
#'                                faceting       = TRUE,
#'                                limits         = TRUE,
#'                                alpha          = 0.15,
#'                                line_size      = 0.5,
#'                                x_axis_text    = 7,
#'                                interactive    = FALSE)
#'
#' @export

draw_timeseries_scatterplot <- function(data, y_var, grouping_var_1, grouping_var_1_type = "date-time",
                                        grouping_var_2, faceting = FALSE, limits = FALSE,
                                        date_breaks = "1 month", date_labels = "%b %y", analysis_desc_label = NULL,
                                        x_axis_text_size = 11, point_size = 1, alpha = 0.3, line_size = 1,  interactive = TRUE) {

  # 1. Tidy Eval ----
  y_var_expr          <- rlang::enquo(y_var)
  grouping_var_1_expr <- rlang::enquo(grouping_var_1)
  grouping_var_2_expr <- rlang::enquo(grouping_var_2)


  # 2. Plotting function - ggplot2::geom_jitter and ggplot2::geom_line ----
  # 2.1 Only grouping_var_1 ----
  if (missing(grouping_var_2)) {

    if (grouping_var_1_type == "date-time") {

      means_tbl <- data %>%
        dplyr::group_by(!!grouping_var_1_expr) %>%
        dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(moving_range = abs(dplyr::lag(mean) - mean)) %>%
        dplyr::mutate(lcl = mean(mean) - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
        dplyr::mutate(ucl = mean(mean) + 2.66 * mean(moving_range, na.rm = TRUE))

      data <- data %>%
        dplyr::left_join(means_tbl) %>%
        dplyr::mutate(overall_mean = mean(!!y_var_expr, na.rm = TRUE))


      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr))

      if (limits) {
        plot <- plot + ggplot2::geom_line(ggplot2::aes(!!grouping_var_1_expr, overall_mean), color = "grey60", size = 0.5, alpha = 0.7, linetype = "dashed")
      }

      plot <- plot +
        ggplot2::geom_jitter(color = "#304269", width = 2, height = 0, alpha = alpha, size = point_size) +
        ggplot2::geom_line(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#304269", size = line_size, alpha = 1) +
        #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
        ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
        sherlock::theme_sherlock()
    }

    if (grouping_var_1_type == "factor") {

      data <- data %>%
        dplyr::mutate(!!grouping_var_1_expr := (!!grouping_var_1_expr) %>% as.factor() %>% forcats::fct_inorder())


      means_tbl <- data %>%
        dplyr::group_by(!!grouping_var_1_expr) %>%
        dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(moving_range = abs(dplyr::lag(mean) - mean)) %>%
        dplyr::mutate(lcl = mean(mean) - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
        dplyr::mutate(ucl = mean(mean) + 2.66 * mean(moving_range, na.rm = TRUE))

      data <- data %>%
        dplyr::left_join(means_tbl)


      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, group = 1)) +
        ggplot2::geom_jitter(color = "#304269", width = 0.05, height = 0, alpha = alpha, size = point_size) +
        #stat_summary(fun = mean, geom = "point") +
        ggplot2::stat_summary(fun = mean, geom = "line", color = "#304269", color = "#304269", size = line_size, alpha = 1) +
        #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
        sherlock::theme_sherlock()
    }

    if (grouping_var_1_type == "numeric") {

      data <- data %>%
        dplyr::group_by(!!grouping_var_1_expr) %>%
        dplyr::mutate(index = n())

      means_tbl <- data %>%
        dplyr::group_by(!!grouping_var_1_expr) %>%
        dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(moving_range = abs(dplyr::lag(mean) - mean)) %>%
        dplyr::mutate(lcl = mean(mean) - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
        dplyr::mutate(ucl = mean(mean) + 2.66 * mean(moving_range, na.rm = TRUE))

      data <- data %>%
        dplyr::left_join(means_tbl)


      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr)) +
        ggplot2::geom_jitter(color = "#304269", width = 0.05, height = 0, alpha = alpha, size = point_size) +
        #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
        sherlock::theme_sherlock()
    }



  } else {
    # 2.2 Grouping_var_1 and grouping_var_2 ----
    # 2.2.1 Faceting ----
    if (faceting) {

      if (grouping_var_1_type == "date-time") {

        overall_mean_tbl <- data %>%
          dplyr::group_by(!!grouping_var_2_expr) %>%
          dplyr::mutate(overall_mean = mean(!!y_var_expr, na.rm = TRUE))


        means_tbl <- data %>%
          dplyr::group_by(!!grouping_var_1_expr, !!grouping_var_2_expr) %>%
          dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(moving_range = abs(dplyr::lag(mean) - mean))
        # dplyr::mutate(lcl = overall_mean - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
        # dplyr::mutate(ucl = overall_mean + 2.66 * mean(moving_range, na.rm = TRUE))


        data <- data %>%
          dplyr::left_join(means_tbl) %>%
          dplyr::group_by(!!grouping_var_2_expr) %>%
          dplyr::mutate(overall_mean = mean(!!y_var_expr, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(lcl = overall_mean - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
          dplyr::mutate(ucl = overall_mean + 2.66 * mean(moving_range, na.rm = TRUE))


        plot <- data %>%
          ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_2_expr))

        if (limits) {
          plot <- plot + ggplot2::geom_line(ggplot2::aes(!!grouping_var_1_expr, overall_mean), color = "grey60", size = 0.5, alpha = 0.7, linetype = "dashed")
        }

        plot <- plot +
          ggplot2::geom_jitter(color = "#304269", width = 2, height = 0, alpha = alpha, size = point_size) +
          ggplot2::geom_line(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#304269", size = line_size, alpha = 1) +
          #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
          ggplot2::facet_grid(rows = ggplot2::vars(!!grouping_var_2_expr)) +
          ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
          sherlock::theme_sherlock() +
          sherlock::scale_color_sherlock()
      }

      if (grouping_var_1_type == "factor") {

        data <- data %>%
          dplyr::mutate(!!grouping_var_1_expr := (!!grouping_var_1_expr) %>% as.factor() %>% forcats::fct_inorder())

        means_tbl <- data %>%
          dplyr::group_by(!!grouping_var_1_expr, !!grouping_var_2_expr) %>%
          dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE)) %>%
          dplyr::mutate(moving_range = abs(dplyr::lag(mean) - mean)) %>%
          dplyr::mutate(lcl = mean(mean) - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
          dplyr::mutate(ucl = mean(mean) + 2.66 * mean(moving_range, na.rm = TRUE)) %>%
          dplyr::ungroup()

        data <- data %>%
          dplyr::left_join(means_tbl)

        plot <- data %>%
          ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_2_expr)) +
          ggplot2::geom_jitter(color = "#304269", width = 0.05, height = 0, alpha = alpha, size = point_size) +
          #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
          ggplot2::facet_grid(rows = ggplot2::vars(!!grouping_var_2_expr)) +
          sherlock::theme_sherlock() +
          sherlock::scale_color_sherlock()
      }



      # 2.2.2 No faceting ----
    } else {
      if (grouping_var_1_type == "date-time") {

        means_tbl <- data %>%
          dplyr::group_by(!!grouping_var_1_expr) %>%
          dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(moving_range = abs(dplyr::lag(mean) - mean)) %>%
          dplyr::mutate(lcl = mean(mean) - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
          dplyr::mutate(ucl = mean(mean) + 2.66 * mean(moving_range, na.rm = TRUE))

        data <- data %>%
          dplyr::left_join(means_tbl) %>%
          dplyr::mutate(overall_mean = mean(!!y_var_expr, na.rm = TRUE))

        plot <- data %>%
          ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_2_expr))

        if (limits) {
          plot <- plot + ggplot2::geom_line(ggplot2::aes(!!grouping_var_1_expr, overall_mean), color = "grey60", size = 0.5, alpha = 0.7, linetype = "dashed")
        }

        plot <- plot +
          ggplot2::geom_jitter(width = 2, height = 0, alpha = alpha, size = point_size) +
          ggplot2::geom_line(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#304269", size = line_size, alpha = 1) +
          #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
          ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
          sherlock::theme_sherlock() +
          sherlock::scale_color_sherlock()
      }

      if (grouping_var_1_type == "factor") {

        data <- data %>%
          dplyr::mutate(!!grouping_var_1_expr := (!!grouping_var_1_expr) %>% as.factor() %>% forcats::fct_inorder())


        means_tbl <- data %>%
          dplyr::group_by(!!grouping_var_1_expr) %>%
          dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(moving_range = abs(dplyr::lag(mean) - mean)) %>%
          dplyr::mutate(lcl = mean(mean) - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
          dplyr::mutate(ucl = mean(mean) + 2.66 * mean(moving_range, na.rm = TRUE))

        data <- data %>%
          dplyr::left_join(means_tbl)

        plot <- data %>%
          ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_2_expr)) +
          ggplot2::geom_jitter(width = 0.05, height = 0, alpha = alpha, size = point_size) +
          #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
          sherlock::theme_sherlock() +
          sherlock::scale_color_sherlock()
      }
    }
  }


  # 3. Plot theme ----
  plot <- plot +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(color = "grey50", face = "bold", size = 11),
      panel.border     = ggplot2::element_rect(colour = "grey70"),
      panel.spacing    = ggplot2::unit(1, "lines"),
      axis.title.x     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.title.y     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.text.x      = ggplot2::element_text(size = x_axis_text_size, color = "grey50"),
      axis.text.y      = ggplot2::element_text(size = 11, color = "grey50"),
      legend.title     = ggplot2::element_text(color = "grey50", size = 11),
      legend.text      = ggplot2::element_text(color = "grey50", size = 11),
      plot.title       = ggplot2::element_text(hjust = 0, size = 16, color = "grey50")) +
    ggplot2::labs(
      title    = "Timeseries Scatterplot",
      subtitle = analysis_desc_label
    )


  # # 5. X axis text ----
  # if(x_axis_text == "normal") {
  #   plot <- plot +
  #     ggplot2::theme(axis.text.x = ggplot2::element_text(size = 11, color = "grey50"))
  # }
  #
  # if(x_axis_text == "small") {
  #   plot <- plot +
  #     ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7, color = "grey50"))
  # }
  #
  # if(x_axis_text == "none") {
  #   plot <- plot +
  #     ggplot2::theme(axis.text.x = ggplot2::element_blank())
  # }



  # 6. Plotting limits ----
  if (limits) {

    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(!!grouping_var_1_expr, lcl), color = "#700808", size = 0.5, alpha = 0.3) +
      ggplot2::geom_line(ggplot2::aes(!!grouping_var_1_expr, ucl), color = "#700808", size = 0.5, alpha = 0.3)
  }


  # 7. Interactivity with plotly ----
  if (interactive) {
    plot <- plotly::ggplotly(plot)
  } else {
    plot
  }

  # only used for debugging
  # return(data)
  return(plot)
}
