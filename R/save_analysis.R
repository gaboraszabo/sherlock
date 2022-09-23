#' Save Analysis
#'
#' @description
#' Saves analysis results, both data and plot, into an .xlsx file
#'
#' @param data Data to be saved (required)
#' @param plot Plot to be saved (optional)
#' @param filename Name of the Excel file in a string format without the .xlsx extension. Example: "analysis_results" (required)
#' @param filepath Path for the file. Example: "Documents/" (required)
#'
#' @return An Excel file
#'
#' @export


save_analysis <- function(data, plot, filename, filepath) {

  # 1. CREATE EXCEL WORKBOOK
  wb <- createWorkbook()

  # 2. CREATE WORKSHEET(S)
  addWorksheet(wb = wb, sheetName = "data")
  if (!missing(plot)) {
    addWorksheet(wb = wb, sheetName = "plot")
  }

  # 3. ADD IMAGE TO WORKSHEET "PLOT"
  if (!missing(plot)) {
    print(plot)
    insertPlot(wb = wb, sheet = "plot")
  }

  # 4. ADD DATA TO WORKSHEET "DATA"
  writeData(wb = wb, sheet = "data", x = data)
  saveWorkbook(wb = wb, file = paste0(filepath, filename, ".xlsx"), overwrite = TRUE)
}
