library(tidyverse)
library(readxl)
library(janitor)
library(zoo)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Iraq")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "Iraq 2022 HNO Final Intersectoral & Cluster PIN Estimates - Updated 20211129.xlsx"
)

df_ocha_clusters_overall_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Overall-District"
) %>% mutate(population_group = "Overall")

df_ocha_clusters_in_camp_idps_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "In-Camp-IDPs-District"
) %>% mutate(population_group = "In-Camp IDPs")

df_ocha_clusters_out_of_camp_idps_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Out-of-Camp-IDPs-District"
) %>% mutate(population_group = "Out-of-Camp IDPs")

df_ocha_clusters_returnees_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Returnees-District"
) %>% mutate(population_group = "Returnees")


# Needs some cleaning of the header
# https://paul.rbind.io/2019/02/01/tidying-multi-header-excel-data-with-r/

sheet_name <- "Gov. PIN & AcutePIN"

head1 <- read_excel(ocha_fp, col_names = TRUE, sheet = sheet_name, skip = 2) %>%
  names() %>%
  str_replace("...\\d", NA_character_)

head1 <- tibble(head1) %>%
  mutate(head1 = zoo::na.locf0(head1)) %>%
  pull()

head2 <- read_excel(ocha_fp, col_names = TRUE, sheet = sheet_name, skip = 3) %>%
  names() %>%
  str_remove("...\\d+")

headers <- map_chr(1:length(head1), ~ {
  case_when(
    !is.na(head1[.x]) & !is.na(head2[.x]) ~ paste(head1[.x], head2[.x], sep = "_"),
    TRUE ~ head2[.x]
  )
})

df_ocha_is_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Gov. PIN & AcutePIN",
  col_names = headers
) %>% mutate(sector = "all")

########################
#### DATA WRANGLING ####
########################
# one code to bring them all

# saving pcodes and names to ensure unique names at end
df_pcodes <- df_ocha_clusters_overall_raw %>%
  select(
    adm1_pcode = admin1Pcode,
    adm2_pcode = admin2Pcode,
    adm1_en = gov_name,
    adm2_en = dist_name,
  ) %>%
  distinct()

# Do a vertical join before pviot
df_ocha_clusters_raw <- bind_rows(
  df_ocha_clusters_overall_raw,
  df_ocha_clusters_in_camp_idps_raw,
  df_ocha_clusters_out_of_camp_idps_raw,
  df_ocha_clusters_returnees_raw
)


# Pivot to make clusters and PINs
df_ocha_clusters <- df_ocha_clusters_raw %>%
  # Swap the prefix & suffix of the column names, so that pivot longer
  # works the right way
  rename_with(~ gsub("(\\w+)_(pin|acute|sev)", "\\2_\\1", .x)) %>%
  pivot_longer(
    cols = starts_with("pin") | starts_with("acute") | starts_with("sev"),
    names_to = c(".value", "sector"),
    names_pattern = "(\\w+)_(\\w+)"
  ) %>%
  # Drop unused columns
  select(-"mcna", -"pop_num", -"pop_sch", -"pop_0-17", -"acute", -"sev") %>%
  rename(
    adm1_pcode = admin1Pcode, adm2_pcode = admin2Pcode,
    adm1_en = gov_name, adm2_en = dist_name
  ) %>%
  # There are some weird duplicate rows with no data, drop them
  drop_na(adm1_en) %>%
  mutate(adm1_en = na_if(adm1_en, "Total"))

# Pivoting the IS table
# Actually first need to fix the column names
df_ocha_is <- df_ocha_is_raw %>%
  rename_with(~ gsub("(.*)_(.*)", "\\2_\\1", .x)) %>%
  pivot_longer(
    cols = starts_with("Population") | starts_with("PIN") | starts_with("Acute PIN"),
    names_to = c(".value", "population_group"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  # Rename PIN column to match other data, goverornate to join with pcodes
  rename(pin = PIN, adm1_en = Governorate) %>%
  # Get the admin regions
  left_join(df_pcodes) %>%
  # Drop columns that aren't required
  select(-"Population", -"Acute PIN") %>%
  # Remove "total" from adm1_en
  mutate(adm1_en = na_if(adm1_en, "Total"))

############################
#### GENERATE FULL DATA ####
############################
# and in the darkness bind them

df_irq <- bind_rows(
  df_ocha_clusters,
  df_ocha_is,
) %>%
  mutate(
    adm0_pcode = "IRQ",
    adm0_en = "irq",
    .before = adm1_en
  )

# write_csv(
#   df_irq,
#   file_paths$save_path
# )
