#' Create Project Folder
#'
#' @description
#' Creates a project folder on your computer
#'
#' @param folder_name Set name of the folder. Examples: "Analysis_20221212", "01_application" (required)
#' @param path Set path for folder. Example: "R/Projects/" (required)
#' @param subfolders Set type of subfolder structure. Options are "generic" or "shiny". The "generic" option has the following subfolders: "01_data", "02_reports", "03_images", "04_scripts" and "05_misc". The "shiny" option has the following subfolders: "data", "css", "wwww", "images" and "scripts". By default, it is set to "generic". (optional)
#'
#' @return A project folder and sub-folder structure and corresponding .Rproj file on your computer
#'
#' @export


create_project_folder <- function(folder_name,
                                  path,
                                  subfolders = "generic") {

  # SUBFOLDER STRUCTURES ----
  # 1. generic ----
  if (subfolders == "generic") {
    folder_structure <- c("01_data",
                          "02_reports",
                          "03_images",
                          "04_scripts",
                          "05_misc")
  }
  # 2. shiny app ----
  if (subfolders == "shiny") {
    folder_structure <- c("data",
                          "css",
                          "www",
                          "images",
                          "scripts")
  }




  folder_path <- paste0(path, "/", folder_name)

  fs::dir_create(folder_path)



  folder_path_str <- rep(paste0(folder_path, "/"), times = length(folder_structure))

  folder_structure_paths <- purrr::map2_chr(.x = folder_path_str, .y = folder_structure, .f = paste0)

  purrr::map(.x = folder_structure_paths, .f = fs::dir_create)

  rstudioapi::initializeProject(path = folder_path)


  return(message(stringr::str_glue("A new project folder has been created at {normalizePath(folder_path)}.")))

}
