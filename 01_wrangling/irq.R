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

df_ocha_is_raw <- read_excel(ocha_fp,
  col_names = TRUE, sheet = "Gov. PIN & AcutePIN", skip = 2
)

head1 <- names(df_ocha_is_raw) %>%
  str_replace("...\\d", NA_character_) %>%
  zoo::na.locf0()

head2 <- df_ocha_is_raw[1, ] %>%
  unlist(use.names = F)

headers <- ifelse(
  !is.na(head1),
  paste(head1, head2, sep = "_"),
  head2
)

df_ocha_is_raw <- df_ocha_is_raw %>%
  rename_with(~headers) %>%
  slice(-1) %>%
  type_convert() %>%
  mutate(sector = "all")

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


# Pivot to make clusters and PINs,
# drop strange empty columns
df_ocha_clusters <- df_ocha_clusters_raw %>%
  pivot_longer(
    cols = ends_with("pin") | ends_with("acute") | ends_with("sev"),
    names_to = c("sector", ".value"),
    names_pattern = "(\\w+)_(\\w+)"
  ) %>%
  select(-c(mcna, pop_num, pop_sch, `pop_0-17`, acute, sev)) %>%
  rename(
    adm1_pcode = admin1Pcode, adm2_pcode = admin2Pcode,
    adm1_en = gov_name, adm2_en = dist_name
  ) %>%
  drop_na(adm1_en) %>%
  mutate(adm1_en = na_if(adm1_en, "Total"))

# Pivoting the IS table
# Actually first need to fix the column names
df_ocha_is <- df_ocha_is_raw %>%
  pivot_longer(
    cols = ends_with("Population") | ends_with("PIN") | ends_with("Acute PIN"),
    names_to = c("population_group", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  rename(pin = PIN, adm1_en = Governorate) %>%
  left_join(df_pcodes, by = "adm1_en") %>%
  select(-c(Population, `Acute PIN`)) %>%
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
