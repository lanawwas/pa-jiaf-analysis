library(tidyverse)
library(readxl)

###################
#### DATA DIRS ####
###################

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths("Nigeria")

############################
#### OCHA PROVIDED DATA ####
############################

fp_ocha = file.path(
    file_paths$ocha_dir,
    "Nigeria HPC 2022 projected_PIN_All_Data_Shared.xlsx"
  )

df_ocha_raw <- read_excel(
  fp_ocha,
  sheet = "PiN_LGA_Sectors",
  range = ("A2:R67"),
)

df_ocha_clusters = df_ocha_raw %>%
  select(adm2_en = LGA, WASH:`PRO-HLP`) %>%
  pivot_longer(
    cols = WASH:`PRO-HLP`,
    names_to = "sector",
    values_to = "pin"
  )

df_ocha_is <- df_ocha_raw %>%
  select(
    adm2_en = LGA,
    pin = `Estimated JIAF PIN\r\n(Severity classes 3, 4 & 5)`
  ) %>%
  mutate(sector = "intersectoral")

################
#### PCODES ####
################

