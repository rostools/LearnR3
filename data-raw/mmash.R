library(here)
library(tidyverse)
source(here("R/functions.R"))

# Download
mmash_link <- "https://physionet.org/static/published-projects/mmash/multilevel-monitoring-of-activity-and-sleep-in-healthy-people-1.0.0.zip"
download.file(mmash_link, destfile = here("data-raw/mmash-data.zip"))

# Unzip
unzip(here("data-raw/mmash-data.zip"),
      exdir = here("data-raw"),
      junkpaths = TRUE)
unzip(here("data-raw/MMASH.zip"),
      exdir = here("data-raw"))

# Remove/tidy up left over files
library(fs)
file_delete(here(c("data-raw/MMASH.zip",
                   "data-raw/SHA256SUMS.txt",
                   "data-raw/LICENSE.txt")))
file_move(here("data-raw/DataPaper"), here("data-raw/mmash"))

# Read in data
user_info_df <- import_multiple_files("user_info.csv", import_user_info)
saliva_df <- import_multiple_files("saliva.csv", import_saliva)










