#' Read in one nurses' stress data file.
#'
#' @param file_path Path to the data file.
#' @param max_rows Maximum number of rows to read.
#'
#' @returns Outputs a data frame/tibble.
#'
read <- function(file_path, max_rows = 10) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}


#' Read in one nurses' stress data file.
#'
#' @param file_path Path to the data file.
#' @param max_rows Maximum number of rows to read.
#'
#' @returns Outputs a data frame/tibble.
#'
read_all <- function(filename) {
  files <- here::here("data-raw/nurses-stress/") |>
    fs::dir_ls(regexp = filename, recurse = TRUE)

  data <- files |>
    purrr::map(read) |>
    purrr::list_rbind(names_to = "file_path_id")

  return(data)
}

#' Get the participant ID from the file path column.
#'
#' @param data Data with `file_path_id` column.
#'
#' @returns A data frame/tibble.
#'
get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        "/stress/[:alnum:]{2}/"
      ) |>
        stringr::str_remove("/stress/") |>
        stringr::str_remove("/"),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)
  return(data_with_id)
}


#' Summarise values in a data frame by a rounded datetime
#'
#' @param data A data frame with at least a `collection_datetime` column and
#'  some numeric columns to summarise.
#'
#' @returns A summarised data frame.
#'
summarise_by_datetime <- function(data) {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        list(mean = mean, sd = sd, median = median)
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}

#' Tidy up the dates in the survey results
#'
#' @param data A data frame of the survey results data.
#'
#' @return A data frame.
tidy_survey_dates <- function(data) {
  tidied <- data |>
    dplyr::mutate(
      date = lubridate::mdy(date),
      start_datetime = lubridate::as_datetime(paste(date, start_time)),
      end_datetime = lubridate::as_datetime(paste(date, end_time)),
      datetime_id = start_datetime,
      .before = start_time
    ) |>
    dplyr::select(-c(date, start_time, end_time, duration))
  return(tidied)
}

#' Pivot survey data to longer format, with only IDs and datetimes
#'
#' @param data A data frame of the data from `tidy_survey_dates()`.
#'
#' @returns A data frame.
survey_to_long <- function(data) {
  longer <- data |>
    dplyr::select(id, datetime_id, start_datetime, end_datetime) |>
    tidyr::pivot_longer(
      c(start_datetime, end_datetime),
      names_to = NULL,
      values_to = "collection_datetime"
    ) |>
    dplyr::group_by(dplyr::pick(-collection_datetime)) |>
    tidyr::complete(
      collection_datetime = seq(
        min(collection_datetime),
        max(collection_datetime),
        by = 60
      )
    ) |>
    dplyr::ungroup()
  return(longer)
}
