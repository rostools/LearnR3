#' Import MMASH user info data file
#'
#' @param file_path Path to user info data file.
#'
#' @return Outputs a data frame/tibble
import_user_info <- function(file_path) {
  info_data <- readr::read_csv(
    file_path,
    col_select = -1,
    col_types = readr::cols(
      readr::col_skip(),
      gender = readr::col_character(),
      weight = readr::col_double(),
      height = readr::col_double(),
      age = readr::col_double()
    ),
    name_repair = snakecase::to_snake_case
  )

  return(info_data)
}

#' Import the MMASH saliva file
#'
#' @param file_path Path to the user saliva data file.
#'
#' @return Outputs a data frame/tibble.
#'
import_saliva <- function(file_path) {
  saliva_data <- readr::read_csv(
    file_path,
    col_select = -1,
    col_types = readr::cols(
      samples = readr::col_character(),
      cortisol_norm = readr::col_double(),
      melatonin_norm = readr::col_double(),
      .delim = ","
    ),
    name_repair = snakecase::to_snake_case
  )
  return(saliva_data)
}

#' Import the MMASH RR file (heart beat-to-beat interval).
#'
#' @param file_path Path to the user RR data file.
#'
#' @return Outputs a data frame/tibble.
#'
import_rr <- function(file_path) {
  rr_data <- readr::read_csv(
    file_path,
    col_select = -1,
    col_types = readr::cols(
      ibi_s = readr::col_double(),
      day = readr::col_double(),
      # Converts to seconds
      time = readr::col_time(format = ""),
      .delim = ","
    ),
    name_repair = snakecase::to_snake_case
  )
  return(rr_data)
}

#' Import the MMASH Actigraph file (accelerometer).
#'
#' @param file_path Path to the user Actigraph data file.
#'
#' @return Outputs a data frame/tibble.
#'
import_actigraph <- function(file_path) {
  actigraph_data <- readr::read_csv(
    file_path,
    col_select = -1,
    col_types = readr::cols(
      axis_1 = readr::col_double(),
      axis_2 = readr::col_double(),
      axis_3 = readr::col_double(),
      steps = readr::col_double(),
      hr = readr::col_double(),
      inclinometer_off = readr::col_double(),
      inclinometer_standing = readr::col_double(),
      inclinometer_sitting = readr::col_double(),
      inclinometer_lying = readr::col_double(),
      vector_magnitude = readr::col_double(),
      day = readr::col_double(),
      time = readr::col_time(format = ""),
      .delim = ","
    ),
    name_repair = snakecase::to_snake_case
  )
  return(actigraph_data)
}

#' Import multiple MMASH data files and merge into one data frame.
#'
#' @param file_pattern Pattern for which data file to import.
#' @param import_function Function to import the data file.
#'
#' @return A single data frame/tibble.
#'
import_multiple_files <- function(file_pattern, import_function) {
  data_files <- fs::dir_ls(here::here("data-raw/mmash/"),
    regexp = file_pattern,
    recurse = TRUE
  )

  combined_data <- purrr::map(data_files, import_function) |>
    purrr::list_rbind(names_to = "file_path_id") |>
    extract_user_id()
  return(combined_data)
}


#' Extract user ID from data with file path column.
#'
#' @param imported_data Data with `file_path_id` column.
#'
#' @return A data.frame/tibble.
#'
extract_user_id <- function(imported_data) {
  extracted_id <- imported_data |>
    dplyr::mutate(
      user_id = stringr::str_extract(
        file_path_id,
        "user_[1-9][0-9]?"
      ),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)
  return(extracted_id)
}
