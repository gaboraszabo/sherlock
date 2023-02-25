#' Load Files
#'
#' @description
#' Reads a series of either .xlsx or .csv files into a table. Particularly useful when reading in multiple files having the same variables, for example reading in data from an experiment where data was logged and saved separately for each individual unit. Integration of a custom data cleaning function.
#'
#' @param folder Folder where the files to be read in are located (required)
#' @param filetype Set whether to read in .xlsx or .csv files. It takes either ".xlsx" or ".csv". By default, it is set to ".csv" (required)
#' @param data_cleaning_function Add a custom data cleaning function built for individual files. Use no brackets when referencing the function, for example clean_data_from_data_logger. The function being added must be saved in the environment (optional)
#' @param id_by_filename Logical. If set to TRUE, the output will contain a column, storing the name of each file being read in. Ideally, the names of the individual files should be pertinent to their content, e.g. if 20 files are being read in with experimental data from parts 1 through 20, the files should be named 1-20. (optional)
#' @param id_col_name Specify a name for the .id column. By default, it is set to "index" (optional)
#'
#' @return Returns data in the form of a `tibble` object.
#'
#' @export

load_files <- function(folder, filetype = ".csv", data_cleaning_function = NULL,
                       id_by_filename = FALSE, id_col_name = "index") {


  # GET PATHS FOR INDIVIDUAL FILES, REMOVE FILES WITH EXTENSIONS NOT MATCHING FILETYPE ARG ----
  paths <- fs::dir_info(path = folder) %>%
    dplyr::filter(path %>% stringr::str_detect(filetype)) %>%
    dplyr::pull(path)

  # NAMES OF INDIVIDUAL FILES WITHOUT EXTENSION ----
  filenames_str_no_extension <- purrr::map_chr(.x = paths, .f = fs::path_file) %>% stringr::str_remove_all(filetype)


  if (filetype == ".xlsx") {
    path <- fs::dir_info(folder) %>%
      dplyr::filter(stringr::str_detect(path, ".xlsx")) %>%
      dplyr::pull(path)

    list_of_files <- purrr::map(.x = path, .f = openxlsx::read.xlsx)


    if (!is.null(data_cleaning_function)) {

      # ADD FILENAME AS .ID ----
      if (id_by_filename) {
        list_of_files <- list_of_files %>% purrr::set_names(filenames_str_no_extension)
      }

      data <- purrr::map_dfr(.x  = list_of_files,
                             .f  = data_cleaning_function,
                             .id = id_col_name) %>%
        dplyr::as_tibble()
    }

    if (is.null(data_cleaning_function)) {

      if (id_by_filename) {
        list_of_files <- list_of_files %>% purrr::set_names(filenames_str_no_extension)
      }

      data <- purrr::map_dfr(.x  = list_of_files,
                             .f  = dplyr::as_tibble,
                             .id = id_col_name)
    }
  }


  if (filetype == ".csv") {
    path <- fs::dir_info(folder) %>%
      dplyr::filter(stringr::str_detect(path, ".csv")) %>%
      dplyr::pull(path)

    list_of_files <- purrr::map(.x = path, .f = readr::read_csv)


    if (!is.null(data_cleaning_function)) {

      # ADD FILENAME AS .ID ----
      if (id_by_filename) {
        list_of_files <- list_of_files %>% purrr::set_names(filenames_str_no_extension)
      }

      data <- purrr::map_dfr(.x  = list_of_files,
                             .f  = data_cleaning_function,
                             .id = id_col_name)
    }

    if (is.null(data_cleaning_function)) {

      if (id_by_filename) {
        list_of_files <- list_of_files %>% purrr::set_names(filenames_str_no_extension)
      }

      data <- purrr::map_dfr(.x  = list_of_files,
                             .f  = dplyr::as_tibble,
                             .id = id_col_name)
    }
  }


  if (!(filetype %in% c(".xlsx", ".csv"))) {
    warning("Wrong filetype entered. You need to enter either '.xlsx' or '.csv'.")
  }

  return(data)

}
