library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Burundi")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "Burundi HNO 2022 jiaf1.1 Calculation File-OCHAL17243904.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 2,
  sheet = "PIN Overview-Expert Judgement"
) %>%
  clean_names() %>%
  drop_na(x1)

df_ocha_pcode_extract <- read_excel(
  ocha_fp,
  sheet = "Step 5-Severity"
) %>%
  clean_names()

########################
#### DATA WRANGLING ####
########################

df_cleaned <- df_ocha_raw %>%
  pivot_longer(
    cols = c(abris:secal, pin_final),
    names_to = "sector",
    values_to = "pin") %>%
  transmute(
    adm0_en = "Burundi",
    adm0_pcode = "BDI",
    adm1_en = province,
    adm1_pcode = df_ocha_pcode_extract$adm1_pcode[match(province, df_ocha_pcode_extract$adm1_state)],
    population_group = gsub("[0-9]_", "", population),
    sector = ifelse(sector == "pin_final", "intersectoral", sector),
    pin,
    source = "ocha",
    sector_general = ifelse(sector == "intersectoral", "intersectoral", "sectoral")
  )

write_csv(
  df_cleaned,
  file_paths$save_path
)



