#' Select Low-High Units
#'
#' @description
#' Select low-high units manually in a tibble and assign them into groups
#'
#' @param data input dataset (required)
#' @param select_units_by Set to select units either based on row number or part ID. Options are "row_number" and "part_id". By default, it is set to "row_number". (required)
#' @param lowest_units A numerical or character vector of the lowest units selected. Examples: c(1, 6, 8, 12), c("part5", "part45", "part9", "part23"). (required)
#' @param highest_units A numerical or character vector of the lowest units selected. Examples: c(1, 6, 8, 12), c("part5", "part45", "part9", "part23"). (required)
#' @param part_id_col Set column for part id. Only to be used when select_units_by is set to "part_id".
#'
#' @return A tibble object filtered down to the low-high units selected
#'
#' @export

select_low_high_units_manual <- function(data, select_units_by = "row_number", lowest_units, highest_units, part_id_col) {

  # Tidy Eval ----
  part_id_col_expr <- rlang::enquo(part_id_col)


  # Data Transformation ----
  if (select_units_by == "row_number") {

    lowest_units <- data %>%
      dplyr::slice(lowest_units) %>%
      dplyr::mutate(low_high = "low") %>%
      dplyr::mutate(color_label = "red")

    highest_units <- data %>%
      dplyr::slice(highest_units) %>%
      dplyr::mutate(low_high = "high") %>%
      dplyr::mutate(color_label = "black")
  }

  if (select_units_by == "part_id") {

    data <- data %>%
      dplyr::mutate(!!part_id_col_expr := !!part_id_col_expr %>% as.integer())

    lowest_units <- data %>%
      dplyr::filter(!!part_id_col_expr %in% lowest_units) %>%
      dplyr::mutate(low_high = "low") %>%
      dplyr::mutate(color_label = "red")

    highest_units <- data %>%
      dplyr::filter(!!part_id_col_expr %in% highest_units) %>%
      dplyr::mutate(low_high = "high") %>%
      dplyr::mutate(color_label = "black")
  }


  low_high_tbl <- dplyr::bind_rows(highest_units, lowest_units)

  low_high_tbl <- low_high_tbl %>%
    dplyr::group_by(low_high) %>%
    dplyr::mutate(Pair = 1:n()) %>%
    dplyr::ungroup()

  # REARRANGE COLUMNS ----
  if (missing(part_id_col)) {
    low_high_tbl <- low_high_tbl %>%
      dplyr::select(Pair, dplyr::everything())
  } else {
    low_high_tbl <- low_high_tbl %>%
      dplyr::select(Pair, !!part_id_col_expr, dplyr::everything())
  }


  return(low_high_tbl)
}
