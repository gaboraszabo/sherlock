#' Select Low-High Units
#'
#' @description
#' Automatically selects low-high units in a tibble as well as assigns them into groups
#'
#' @param data input dataset (required)
#' @param var variable of interest (required)
#' @param number_of_pairs Number of low-high pairs to be created. Takes a numeric value (required)
#'
#' @return A tibble object filtered down to the low-high units selected
#'
#' @export

select_low_high_units <- function(data, var, number_of_pairs) {

  # Tidy Eval ----
  var_expr <- rlang::enquo(var)


  # Data Transformation ----
  data <- data %>%
    dplyr::arrange(dplyr::desc(!!var_expr)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(rank, dplyr::everything()) %>%
    dplyr::mutate(low_high = dplyr::case_when(rank <= number_of_pairs ~ "high",
                                              rank > n()-number_of_pairs ~ "low",
                                              TRUE ~ "mid")) %>%
    dplyr::mutate(color_label = dplyr::case_when(low_high == "high" ~ "black",
                                                 low_high == "low" ~ "red",
                                                 TRUE ~ "#3971CB")) %>%
    dplyr::filter(low_high %in% c("low", "high")) %>%
    dplyr::group_by(low_high) %>%
    dplyr::slice_sample(n = number_of_pairs, replace = FALSE) %>%
    dplyr::mutate(Pair = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(Pair, dplyr::everything(), -rank)

  return(data)

}
