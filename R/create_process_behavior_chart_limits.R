#' Create Process Behavior Chart Limits
#'
#' @description
#' Creates limits (LCL, UCL) and related calculations (moving range, LCL, UCL) for a Process Behavior Chart and stores them in separate columns
#'
#' @param data input dataset to be plotted (required)
#' @param y_var Y variable to be plotted on Y axis (required)
#' @param grouping_var Variable to group by (optional)
#'
#' @return A tibble with columns created to store values for the moving range, average, UCL and LCL
#'
#' @export


create_process_behavior_chart_limits <- function(data, y_var, grouping_var) {

  y_var_expr        <- rlang::enquo(y_var)
  grouping_var_expr <- rlang::enquo(grouping_var)

  if (missing(grouping_var)) {
    data <- data %>%
      dplyr::mutate(Average = mean(!!y_var_expr)) %>%
      dplyr::mutate(Moving_Range = abs(dplyr::lag(!!y_var_expr) - !!y_var_expr)) %>%
      dplyr::mutate(lcl = mean(Average) - 2.66 * mean(Moving_Range, na.rm = TRUE)) %>%
      dplyr::mutate(ucl = mean(Average) + 2.66 * mean(Moving_Range, na.rm = TRUE))
  }

  if (!missing(grouping_var)) {
    data <- data %>%
      dplyr::group_by(!!grouping_var_expr) %>%
      dplyr::mutate(Average = mean(!!y_var_expr)) %>%
      dplyr::mutate(Moving_Range = abs(dplyr::lag(!!y_var_expr) - !!y_var_expr)) %>%
      dplyr::mutate(lcl = mean(Average) - 2.66 * mean(Moving_Range, na.rm = TRUE)) %>%
      dplyr::mutate(ucl = mean(Average) + 2.66 * mean(Moving_Range, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }

  return(data)
}
