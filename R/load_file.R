#' Load File
#'
#' @description
#' Reads either an .xlsx or a .csv file into a table
#'
#' @param path path for the file (required)
#' @param filetype set whether to read an .xlsx file or a .csv file. It takes either ".xlsx" or ".csv". By default, it is set to ".xlsx" (optional)
#' @param col_names Either TRUE, FALSE or a character vector of column names. If TRUE, the first row of the input will be used as the column names, and will not be included in the data frame. If FALSE, column names will be generated automatically: X1, X2, X3 etc. If col_names is a character vector, the values will be used as the names of the columns, and the first row of the input will be read into the first row of the output data frame.
#'
#' @return Returns data in the form of a `tibble` object.
#'
#' @export

load_file <- function(path, filetype = ".xlsx", col_names = TRUE) {

  if (filetype == ".xlsx") {
    data <- openxlsx::read.xlsx(xlsxFile = path, colNames = col_names) %>% dplyr::as_tibble()
  }

  if (filetype == ".csv") {
    data <-  readr::read_csv(file = path, col_names = col_names) %>% dplyr::as_tibble()
  }

  if (filetype == ".txt") {
    data <-  readr::read_table(file = path, col_names = col_names) %>% dplyr::as_tibble()
  }


  if (!(filetype %in% c(".xlsx", ".csv", ".txt"))) {
    warning("Wrong filetype entered. You need to enter either '.xlsx', '.csv' or '.txt'.")
  }

  return(data)

}
