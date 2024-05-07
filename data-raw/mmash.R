library(here)
library(fs)
library(tidyverse)
source(here("R/functions.R"))

# Download
mmash_link <- r3::mmash_data_link
# download.file(mmash_link, destfile = here("data-raw/mmash-data.zip"))

# Remove leftover folder so unzipping is always clean
# dir_delete(here("data-raw/mmash"))

# Unzip
unzip(here("data-raw/mmash-data.zip"),
  exdir = here("data-raw"),
  junkpaths = TRUE
)
Sys.sleep(1)
unzip(here("data-raw/MMASH.zip"),
  exdir = here("data-raw")
)

# Remove/tidy up left over files
file_delete(here(c(
  "data-raw/MMASH.zip",
  "data-raw/SHA256SUMS.txt",
  "data-raw/LICENSE.txt"
)))
file_move(here("data-raw/DataPaper"), here("data-raw/mmash"))

# Import multiple files
user_info_df <- import_multiple_files("user_info.csv", import_user_info)
saliva_df <- import_multiple_files("saliva.csv", import_saliva)
rr_df <- import_multiple_files("RR.csv", import_rr)
actigraph_df <- import_multiple_files("Actigraph.csv", import_actigraph)

# Summarise RR and actigraph data
summarised_rr_df <- rr_df |>
  group_by(file_path_id, day) |>
  summarise(across(ibi_s, list(
    mean = \(x) mean(x, na.rm = TRUE),
    sd = \(x) sd(x, na.rm = TRUE)
  )), .groups = "drop")

summarised_actigraph_df <- actigraph_df |>
  group_by(file_path_id, day) |>
  # These statistics will probably be different for you
  summarise(
    across(hr, list(
      mean = \(x) mean(x, na.rm = TRUE),
      sd = \(x) sd(x, na.rm = TRUE)
    )),
    .groups = "drop"
  )
