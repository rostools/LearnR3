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

