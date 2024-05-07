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
