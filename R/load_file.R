#' Load File
#'
#' @description
#' Reads either an .xlsx or a .csv file into a table
#'
#' @param path path for the file (required)
#' @param filetype set whether to read an .xlsx file or a .csv file. It takes either ".xlsx" or ".csv". By default, it is set to ".xlsx" (optional)
#'
#' @return Returns data in the form of a `tibble` object.
#'
#' @export

load_file <- function(path, filetype = ".xlsx") {

  if (filetype == ".xlsx") {
    data <- openxlsx::read.xlsx(xlsxFile = path) %>% dplyr::as_tibble()
  }

  if (filetype == ".csv") {
    data <-  readr::read_csv(file = path) %>% dplyr::as_tibble()
  }


  if (!(filetype %in% c(".xlsx", ".csv"))) {
    warning("Wrong filetype entered. You need to enter either '.xlsx' or '.csv'.")
  }

  return(data)

}
