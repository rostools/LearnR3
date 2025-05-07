#' Import data from the DIME study dataset.
#'
#' @param file_path Path to the CSV file.
#'
#' @returns A data frame
#'
import_dime <- function(file_path) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = 100
    )

  return(data)
}



#' Import all DIME CSV files in a folder into one data frame.
#'
#' @param folder_path The path to the folder that has the CSV files.
#'
#' @return A single data frame/tibble.
#'
import_csv_files <- function(folder_path) {
  files <- folder_path |>
    fs::dir_ls(glob = "*.csv")

  data <- files |>
    purrr::map(import_dime) |>
    purrr::list_rbind(names_to = "file_path_id")
  return(data)
}
