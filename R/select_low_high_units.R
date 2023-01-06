#' Select Low-High Units
#'
#' @description
#' Automatically selects low-high units in a tibble as well as assigns them into groups
#'
#' @param data input dataset (required)
#' @param y_var Y variable of interest (required)
#' @param number_of_pairs Number of low-high pairs to be created. Takes a numeric value (required)
#'
#' @return A tibble object filtered down to the low-high units selected
#'
#' @export

select_low_high_units <- function(data, y_var, number_of_pairs) {

  # Tidy Eval ----
  y_var_expr <- enquo(y_var)


  # Data Transformation ----
  data <- data %>%
    arrange(desc(!!y_var_expr)) %>%
    mutate(rank = row_number()) %>%
    select(rank, everything()) %>%
    mutate(low_high = case_when(rank <= number_of_pairs ~ "high",
                                rank > n()-number_of_pairs ~ "low",
                                TRUE ~ "mid")) %>%
    mutate(color_label = case_when(low_high == "high" ~ "black",
                                   low_high == "low" ~ "red",
                                   TRUE ~ "#3971CB")) %>%

    filter(low_high %in% c("low", "high")) %>%
    group_by(low_high) %>%
    slice_sample(n = number_of_pairs, replace = FALSE) %>%
    mutate(Pair = 1:n()) %>%
    ungroup() %>%
    select(Pair, everything(), -rank)

  return(data)

}