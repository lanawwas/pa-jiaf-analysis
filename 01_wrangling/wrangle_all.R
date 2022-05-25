library(tidyverse)
library(renv)
source(here::here("99_helpers", "helpers.R"))

# get all filepaths
file_paths <- get_paths_analysis()

# Run all file wrangling for countries
wrangle_files <- list.files("01_wrangling")
wrangle_files <- wrangle_files[wrangle_files != "wrangle_all.R"]

walk(
  wrangle_files,
  \(x) {
    print(x)
    run(
      file.path(
        "01_wrangling",
        x
      )
    )
  }
)

# Sectoral PiNs

sectoral_df <- map_dfr(
  list.files(
    file_paths$input_dir,
    pattern = ".csv",
    full.names = TRUE
  ),
  read_csv
) %>%
  mutate(
    sector_group = ifelse(
      source == "cluster",
      "sectoral_cluster",
      sector_general
    ),
    .before = pin
  )

sectoral_df %>%
  write_csv(
    file.path(
      file_paths$input_dir,
      "2022_sectoral_pins.csv"
    )
  )
