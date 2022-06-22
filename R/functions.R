#' Import MMASH user info data file.
#'
#' @param file_path Path to user info data.
#'
#' @return a dataframe / tibble
#'
import_user_info <- function(file_path) {
    info_data <- vroom::vroom(
        file_path,
        col_select = -1,
        col_types = vroom::cols(
            gender = vroom::col_character(),
            weight = vroom::col_double(),
            height = vroom::col_double(),
            age = vroom::col_double(),
            .delim = ","
        ),
        .name_repair = snakecase::to_snake_case
    )
    return(info_data)
}

#' Import the MMASH saliva dataset.
#'
#' @param file_path Path to the user saliva data file.
#'
#' @return Outputs a data frame/tibble.
#'
import_saliva <- function(file_path) {
    saliva_data <- vroom::vroom(
        file_path,
        col_select = -1,
        col_types = vroom::cols(
            samples = vroom::col_character(),
            cortisol_norm = vroom::col_double(),
            melatonin_norm = vroom::col_double(),
            .delim = ","
        ),
        .name_repair = snakecase::to_snake_case
    )
    return(saliva_data)
}

#' Import the MMASH RR dataset (heart beat-to-beat interval).
#'
#' @param file_path Path to the user RR data file.
#'
#' @return Outputs a data frame/tibble.
#'
import_rr <- function(file_path) {
    rr_data <- vroom::vroom(
        file_path,
        col_select = -1,
        col_types = vroom::cols(
            ibi_s = vroom::col_double(),
            day = vroom::col_double(),
            # Converts to seconds
            time = vroom::col_time(format = ""),
            .delim = ","
        ),
        .name_repair = snakecase::to_snake_case
    )
    return(rr_data)
}

#' Import the MMASH Actigraph dataset (accelerometer).
#'
#' @param file_path Path to the user Actigraph data file.
#'
#' @return Outputs a data frame/tibble.
#'
import_actigraph <- function(file_path) {
    actigraph_data <- vroom::vroom(
        file_path,
        col_select = -1,
        col_types = vroom::cols(
            axis_1 = vroom::col_double(),
            axis_2 = vroom::col_double(),
            axis_3 = vroom::col_double(),
            steps = vroom::col_double(),
            hr = vroom::col_double(),
            inclinometer_off = vroom::col_double(),
            inclinometer_standing = vroom::col_double(),
            inclinometer_sitting = vroom::col_double(),
            inclinometer_lying = vroom::col_double(),
            vector_magnitude = vroom::col_double(),
            day = vroom::col_double(),
            time = vroom::col_time(format = ""),
            .delim = ","
        ),
        .name_repair = snakecase::to_snake_case
    )
    return(actigraph_data)
}

import_multiple_files <- function(file_pattern, import_function) {

    data_files <- fs::dir_ls(here::here("data-raw/mmash/"),
                             regexp = file_pattern,
                             recurse = TRUE)

    combined_data <- purrr::map_dfr(data_files, import_function,
                                    .id = "file_path_id")

    return(combined_data)
}

#' Extract user ID from data with file path column.
#'
#' @param imported_data Data with `file_path_id` column.
#'
#' @return A data.frame/tibble.
#'
extract_user_id <- function(imported_data) {
    extracted_id <- imported_data %>%
        dplyr::mutate(user_id = stringr::str_extract(file_path_id,
                                                     "user_[0-9][0-9]?"),
                      .before = file_path_id) %>%
        dplyr::select(-file_path_id)
    return(extracted_id)
}
