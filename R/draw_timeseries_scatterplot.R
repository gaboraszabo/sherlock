#' Draw Timeseries Scatterplot
#'
#' @description
#' Draws a Youden Plot
#'
#' @param data input dataset to be plotted (required)
#' @param y_var Y variable to be plotted on Y axis (required)
#' @param grouping_var_1 Time variable to be plotted on x axis (required)
#' @param grouping_var_1_type Time variable type. Options are "date-time" or "factor"
#' @param grouping_var_2 Additional variable for faceting (optional)
#' @param faceting Set whether to display each group in a separate plot. By default, it is set to FALSE (optional)
#' @param control_limits Logical. If TRUE, process behavior chart (I-MR chart) control limits are plotted. By default, it is set to FALSE  (optional)
#' @param date_breaks Set date breaks. Takes a string, for example "1 week" or "2 days". By default, it is set to "1 month" (optional)
#' @param date_labels Set date labels. Identical to the date labels argument of the scale_x_date() ggplot function (optional)
#' @param x_axis_text X axis text size. The two options are "normal" and "small" (optional)
#' @param alpha Set transparency for the individual observation. Identical to the alpha ggplot argument. By default, it is set to 0.3 (optional)
#' @param interactive Set plot interactivity. By default, it is set to TRUE (optional)
#'
#' @return Either a ggplot or plotly Timeseries Scatterplot object
#'
#' @export

draw_timeseries_scatterplot <- function(data, y_var, grouping_var_1, grouping_var_1_type = "date-time",
                                        grouping_var_2, faceting = FALSE, control_limits = FALSE,
                                        date_breaks = "1 month", date_labels = "%b %y",
                                        x_axis_text = "normal", alpha = 0.3, interactive = TRUE) {

  # 1. Tidy Eval ----
  y_var_expr        <- rlang::enquo(y_var)
  grouping_var_1_expr <- rlang::enquo(grouping_var_1)
  grouping_var_2_expr <- rlang::enquo(grouping_var_2)


  # 2. Adding avg, lcl and ucl columns for control limits ----
  if (control_limits) {

    if (!faceting) {
      data <- data %>%
        dplyr::mutate(avg = mean(!!y_var_expr, na.rm = TRUE)) %>%
        dplyr::mutate(moving_range = abs(dplyr::lag(!!y_var_expr) - !!y_var_expr)) %>%
        dplyr::mutate(lcl = mean(avg) - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
        dplyr::mutate(ucl = mean(avg) + 2.66 * mean(moving_range, na.rm = TRUE))

    } else {
      data <- data %>%
        dplyr::group_by(!!grouping_var_2_expr) %>%
        dplyr::mutate(avg = mean(!!y_var_expr, na.rm = TRUE)) %>%
        dplyr::mutate(moving_range = abs(dplyr::lag(!!y_var_expr) - !!y_var_expr)) %>%
        dplyr::mutate(lcl = mean(avg) - 2.66 * mean(moving_range, na.rm = TRUE)) %>%
        dplyr::mutate(ucl = mean(avg) + 2.66 * mean(moving_range, na.rm = TRUE))

    }
  }

  # 3. Plotting function - ggplot2::geom_jitter and ggplot2::geom_line
  if (missing(grouping_var_2)) {

    if (grouping_var_1_type == "date-time") {

      means_tbl <- data %>%
        dplyr::group_by(!!grouping_var_1_expr) %>%
        dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE))


      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr)) +
        ggplot2::geom_jitter(color = "#304269", width = 2, alpha = alpha, size = 1) +
        ggplot2::geom_line(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#304269", size = 0.5, alpha = 0.6) +
        #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
        ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
        sherlock::theme_sherlock()
    }

    if (grouping_var_1_type == "factor") {

      data <- data %>%
        dplyr::mutate(!!grouping_var_1_expr := (!!grouping_var_1_expr) %>% as.factor() %>% forcats::as_factor())


      means_tbl <- data %>%
        dplyr::group_by(!!grouping_var_1_expr) %>%
        dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE))


      plot <- data %>%
        ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr)) +
        ggplot2::geom_jitter(color = "#304269", width = 0.05, alpha = alpha, size = 1) +
        #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
        sherlock::theme_sherlock()
    }

  } else {

    if (faceting) {

      if (grouping_var_1_type == "date-time") {

        means_tbl <- data %>%
          dplyr::group_by(!!grouping_var_1_expr, !!grouping_var_2_expr) %>%
          dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE))


        plot <- data %>%
          ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_2_expr)) +
          ggplot2::geom_jitter(color = "#304269", width = 2, alpha = alpha, size = 1) +
          ggplot2::geom_line(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#304269", size = 0.5, alpha = 0.6) +
          #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
          ggplot2::facet_grid(rows = ggplot2::vars(!!grouping_var_2_expr)) +
          ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
          sherlock::theme_sherlock() +
          sherlock::scale_color_sherlock()
      }

      if (grouping_var_1_type == "factor") {

        data <- data %>%
          dplyr::mutate(!!grouping_var_1_expr := (!!grouping_var_1_expr) %>% as.factor() %>% forcats::as_factor())


        means_tbl <- data %>%
          dplyr::group_by(!!grouping_var_1_expr, !!grouping_var_2_expr) %>%
          dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE))


        plot <- data %>%
          ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_2_expr)) +
          ggplot2::geom_jitter(color = "#304269", width = 0.05, alpha = alpha, size = 1) +
          #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
          ggplot2::facet_grid(rows = ggplot2::vars(!!grouping_var_2_expr)) +
          sherlock::theme_sherlock() +
          sherlock::scale_color_sherlock()
      }

    } else {
      if (grouping_var_1_type == "date-time") {

        means_tbl <- data %>%
          dplyr::group_by(!!grouping_var_1_expr) %>%
          dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE))


        plot <- data %>%
          ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_2_expr)) +
          ggplot2::geom_jitter(width = 2, alpha = alpha, size = 1) +
          ggplot2::geom_line(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#304269", size = 0.5, alpha = 0.6) +
          #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
          ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
          sherlock::theme_sherlock() +
          sherlock::scale_color_sherlock()
      }

      if (grouping_var_1_type == "factor") {

        data <- data %>%
          dplyr::mutate(!!grouping_var_1_expr := (!!grouping_var_1_expr) %>% as.factor() %>% forcats::as_factor())


        means_tbl <- data %>%
          dplyr::group_by(!!grouping_var_1_expr) %>%
          dplyr::summarize(mean = mean(!!y_var_expr, na.rm = TRUE))


        plot <- data %>%
          ggplot2::ggplot(ggplot2::aes(!!grouping_var_1_expr, !!y_var_expr, color = !!grouping_var_2_expr)) +
          ggplot2::geom_jitter(width = 0.05, alpha = alpha, size = 1) +
          #geom_point(data = means_tbl, ggplot2::aes(!!grouping_var_1_expr, mean), color = "#700808", size = 1.5, alpha = 0.5) +
          sherlock::theme_sherlock() +
          sherlock::scale_color_sherlock()
      }
    }
  }


  # 4. Plot theme ----
  plot <- plot +
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(color = "grey50", face = "bold", size = 11),
      panel.border     = ggplot2::element_rect(colour = "grey70"),
      panel.spacing    = ggplot2::unit(1, "lines"),
      axis.title.x     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.title.y     = ggplot2::element_text(size = 11, color = "grey50"),
      axis.text        = ggplot2::element_text(size = 11, color = "grey50"),
      legend.title     = ggplot2::element_text(color = "grey50", size = 11),
      legend.text      = ggplot2::element_text(color = "grey50", size = 11),
      plot.title       = ggplot2::element_text(hjust = 0, size = 16, color = "grey50")) +
    ggplot2::labs(
      title = "Timeseries Scatterplot"
    )


  # 5. X axis text ----
  if(x_axis_text == "normal") {
    plot <- plot +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 11, color = "grey70"))
  }

  if(x_axis_text == "small") {
    plot <- plot +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7, color = "grey70"))
  }

  if(x_axis_text == "none") {
    plot <- plot +
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
  }



  # 6. Plotting control limits ----
  if (control_limits) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(!!grouping_var_1_expr, avg), color = "grey60", size = 0.5, alpha = 0.7, linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(!!grouping_var_1_expr, lcl), color = "#700808", size = 0.5, alpha = 0.3) +
      ggplot2::geom_line(ggplot2::aes(!!grouping_var_1_expr, ucl), color = "#700808", size = 0.5, alpha = 0.3)
  }


  # 7. Interactivity with plotly ----
  if (interactive) {
    plot <- plotly::ggplotly(plot)
  } else {
    plot
  }

  return(plot)
}
