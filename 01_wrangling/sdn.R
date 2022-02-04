library(tidyverse)
library(readxl)

###################
#### DATA DIRS ####
###################

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths("Sudan")

############################
#### OCHA PROVIDED DATA ####
############################

df_ocha_raw <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "SDN HNO 2022 Baseline -JIAF exercise.xlsx"
  ),
  sheet = "PIN and Severity",
  skip = 6
) %>%
  rename_all(tolower) %>%
  rename(adm1_en = adm1, adm2_pcode = pcode, adm2_en = adm2)

df_ocha_clusters <- df_ocha_raw %>%
  select(-c(pin, sev)) %>%
  pivot_longer(
    cols = contains("_pin_") | contains("_sev_"),
    names_to = c("sector", ".value", "condition", "population_group"),
    names_sep = "_",
  ) %>%
  select(
    adm1_en, adm2_pcode, adm2_en, sector, population_group,
    condition, pin
  )

df_ocha_is <- df_ocha_raw %>%
  select(adm1_en, adm2_pcode, adm2_en, pin) %>%
  mutate(
    pin = as.numeric(pin), sector = "intersectoral",
    population_group = "all", condition = "all"
  )

df_ocha = bind_rows(
  df_ocha_clusters,
  df_ocha_is
) %>% mutate(source = "ocha")

######################
#### CLUSTER DATA ####
######################

# GBV
gbv_fp <- file.path(
  file_paths$cluster_dir,
  "20210927_Sudan_2022_HNO_PiN_FINAL GBV.xlsx"
)

df_gbv_idp <- read_excel(
  gbv_fp,
  sheet = "IDPs",
  skip = 18
) %>% mutate(population_group = "idp")
df_gbv_vul <- read_excel(
  gbv_fp,
  sheet = "Returnees",
  skip = 18
) %>% mutate(population_group = "ret")
df_gbv_ret <- read_excel(
  gbv_fp,
  sheet = "Vulnerable Hosts",
  skip = 18
) %>% mutate(population_group = "vul")
df_gbv_all <- read_excel(
  gbv_fp,
  sheet = "Overall",
  skip = 18
) %>% mutate(population_group = "all")

df_gbv =  bind_rows(
  df_gbv_idp,
  df_gbv_vul,
  df_gbv_ret,
  df_gbv_all
) %>% 
  rename(adm1_en = state_name,
         adm1_pcode = admin1Pcode,
         adm2_en = Locality_name,
         adm2_pcode = admin2Pcode) %>%
select(adm1_en, adm1_pcode, adm2_en, adm2_pcode, pin) %>%
  mutate(sector = "gbv", condition = "all")

df_clusters = bind_rows(
  df_gbv
) %>% mutate(source = "ocha")

############################
#### GENERATE FULL DATA ####
############################

df_sdn <- bind_rows(
  df_ocha_clusters,
  df_ocha_is,
) %>%
  mutate(
    adm0_pcode = "SDN",
    adm0_en = "Sudan",
    .before = adm1_en,
  )

# write_csv(
#   df_sdn,
#   file_paths$save_path
# )
