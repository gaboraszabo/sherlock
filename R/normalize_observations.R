#' Normalize observations
#'
#' @description
#' This function takes an input dataset and normalizes observations
#'
#' @param data input dataset to be plotted (required)
#' @param response response variable, Y (required)
#' @param grouping_var select grouping variable to normalize by (required)
#' @param ref_values add reference (nominal) values. takes a string of values with values appearing in the same order as in grouping variable. string length must be equal to unique values in grouping variable (required)
#'
#' @return A `tibble` object with observations normalized and saved in a new column.
#'
#' @export

normalize_observations <- function(data, response, grouping_var, ref_values) {

  # 1. Tidy Eval
  response_expr <- rlang::enquo(response)
  grouping_var_expr <- rlang::enquo(grouping_var)
  response_ensym <- as.character(rlang::ensym(response))

  grouping_var_distinct <- data %>% dplyr::distinct(!!grouping_var_expr)

  ref_values_tbl <- ref_values %>% dplyr::as_tibble()

  ref_values_tbl_joined <- ref_values_tbl %>%
    dplyr::rename(ref_value = "value") %>%
    dplyr::bind_cols(grouping_var_distinct)


  data <- data %>%
    dplyr::left_join(ref_values_tbl_joined) %>%
    dplyr::mutate(!! stringr::str_glue("{response_ensym}_normalized") := !!response_expr - ref_value)

  return(data)

}
