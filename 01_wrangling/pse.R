library(tidyverse)
library(readxl)
library(stringr)

###################
#### DATA DIRS ####
###################

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths("oPt")

############################
#### OCHA PROVIDED DATA ####
############################

fp_ocha <- file.path(
  file_paths$ocha_dir,
  "master_hno_2022_statistics.xlsx"
)

df_ocha_clusters <- read_excel(
  fp_ocha,
  sheet = "Cluster_PIN_Severity",
  range = "A16:P22"
) %>%
  select(
    sector = Cluster,
    Gaza:H2
  ) %>%
  pivot_longer(
    Gaza:H2,
    names_to = "adm2_en",
    values_to = "pin"
  ) %>%
  mutate(
    pin = pin %>% str_replace("\\s*\\(.+\\)", "")
      %>% str_replace(",", "") %>%
      str_replace("K", "e3") %>%
      str_replace("[^0-9.e]", "") %>%
      as.numeric(),
    adm2_en = case_when(
      adm2_en == "Area A&B" ~ "Area A & B",
      adm2_en == "EJ" ~ "East Jerusalem",
      TRUE ~ adm2_en
    )
  )

df_ocha_is_raw <- read_excel(
  fp_ocha,
  sheet = "PIN_Geography_Severity",
  range = "A1:L10"
)
df_ocha_is <- df_ocha_is_raw %>% select(
  adm2_en = Area,
  pin = PIN
)

df_ocha <- bind_rows(
  df_ocha_clusters,
  df_ocha_is
) %>%
  mutate(source = "ocha", .before = 1)

###############
### PCODES ####
###############

df_pcodes <- df_ocha_is_raw %>%
  select(adm1_en = Region, adm2_en = Area) %>%
  unique()

############################
#### GENERATE FULL DATA ####
############################

df_pse <- bind_rows(
  df_ocha,
) %>%
  left_join(df_pcodes,
    by = "adm2_en",
  ) %>%
  relocate(adm2_en, .before = sector) %>%
  relocate(adm1_en, .before = adm2_en) %>%
  mutate(
    adm0_en = "oPt",
    adm0_pcode = "pse",
    .before = adm1_en,
  )
