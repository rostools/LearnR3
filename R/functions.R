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

#' Get the participant ID from the file path column.
#'
#' @param data Data with `file_path_id` column.
#'
#' @return A data.frame/tibble.
#'
get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        "[:digit:]+\\.csv$"
      ) |>
        stringr::str_remove("\\.csv$") |>
        as.integer(),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)
  return(data_with_id)
}

#' Prepare the date columns in DIME CGM and sleep data for joining.
#'
#' @param data The data that has the datetime column.
#' @param column The datetime column to convert to date and hour.
#'
#' @returns A tibble/data.frame
#'
prepare_dates <- function(data, column) {
  prepared_dates <- data |>
    dplyr::mutate(
      date = lubridate::as_date({{ column }}),
      hour = lubridate::hour({{ column }}),
      .before = {{ column }}
    )
  return(prepared_dates)
}

#' Clean and prepare the CGM data for joining.
#'
#' @param data The CGM dataset.
#'
#' @returns A cleaner data frame.
#'
clean_cgm <- function(data) {
  cleaned <- data |>
    get_participant_id() |>
    prepare_dates(device_timestamp) |>
    dplyr::rename(glucose = historic_glucose_mmol_l) |>
    # You can decide what functions to summarise by.
    summarise_column(glucose, list(mean = mean, sd = sd))
  return(cleaned)
}

#' Summarise a single column based one or more functions.
#'
#' @param data Either the CGM or sleep data in DIME.
#' @param column The column we want to summarise.
#' @param functions One or more functions to apply to the column. If more than one added, use list()
#'
#' @returns
summarise_column <- function(data, column, functions) {
  summarized_data <- data |>
    dplyr::select(-tidyselect::contains("timestamp"), -tidyselect::contains("datetime")) |>
    dplyr::group_by(dplyr::pick(-{{ column }})) |>
    dplyr::summarise(
      dplyr::across(
        {{ column }},
        functions
      ),
      .groups = "drop"
    )
  return(summarized_data)
}

#' Clean and prepare the sleep data for joining.
#'
#' @param data The sleep dataset.
#'
#' @returns A cleaner data frame.
#'
clean_sleep <- function(data) {
  cleaned <- data |>
    get_participant_id() |>
    dplyr::rename(datetime = date) |>
    prepare_dates(datetime) |>
    summarise_column(seconds, list(sum = sum)) |>
    sleep_types_to_wider()
  return(cleaned)
}


#' Convert the participant details data to long and clean it up.
#'
#' @param data The DIME participant details data.
#'
#' @returns A data frame
clean_participant_details <- function(data) {
  cleaned <- data |>
    tidyr::pivot_longer(tidyselect::ends_with("date"), names_to = NULL, values_to = "date") |>
    dplyr::group_by(dplyr::pick(-date)) |>
    tidyr::complete(
      date = seq(min(date), max(date), by = "1 day")
    )

  return(cleaned)
}

#' Convert the sleep types to wide format.
#'
#' @param data The cleaned DIME sleep data.
#'
#' @returns A data frame.
#'
sleep_types_to_wider <- function(data) {
  wider <- data |>
    tidyr::pivot_wider(
      names_from = sleep_type,
      names_prefix = "seconds_",
      values_from = seconds_sum
    )
  return(wider)
}
