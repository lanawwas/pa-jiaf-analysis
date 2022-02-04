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

df_ocha <- bind_rows(
  df_ocha_clusters,
  df_ocha_is
) %>%
  mutate(source = "ocha", .before = 1) %>%
  drop_na(adm2_pcode)

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

df_gbv <- bind_rows(
  df_gbv_idp,
  df_gbv_vul,
  df_gbv_ret,
  df_gbv_all
) %>%
  rename(
    adm1_en = state_name,
    adm1_pcode = admin1Pcode,
    adm2_en = Locality_name,
    adm2_pcode = admin2Pcode
  ) %>%
  select(adm1_en, adm1_pcode, adm2_en, adm2_pcode, pin) %>%
  mutate(sector = "gbv", condition = "all")

# Education
df_edu_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "Sudan - Education PiN calculation - 2022 HNO.xlsx"
  ),
  sheet = "EDU", skip = 15
) %>%
  rename_all(tolower) %>%
  rename(
    adm1_pcode = `p-code admin1`,
    adm1_en = state,
    adm2_pcode = `pcode admin2`,
    adm2_en = locality,
    edu_pin_all = edu_pin
  ) %>%
  select(
    adm1_pcode, adm1_en, adm2_pcode, adm2_en,
    edu_pin_all, edu_pin_idp, edu_pin_ret, edu_pin_vul, edu_pin_ref
  )

df_edu <- df_edu_raw %>%
  pivot_longer(
    cols = contains("_pin_"),
    names_to = c("sector", ".value", "population_group"),
    names_sep = "_",
  ) %>%
  mutate(condition = "all")

# Combine clusters
df_clusters <- bind_rows(
  df_gbv,
  df_edu
) %>% mutate(source = "cluster")


####################
### FILL PCODES ####
####################

# OCHA data missing admin1 pcodes, use the edu data to add them

df_pcodes <- df_edu %>%
  select(adm1_pcode, adm1_en) %>%
  unique()

df_ocha_pcoded <- df_ocha %>%
  left_join(df_pcodes,
    by = "adm1_en"
  ) %>%
  relocate(adm1_pcode, .before = adm1_en)


############################
#### GENERATE FULL DATA ####
############################

df_sdn <- bind_rows(
  df_ocha_pcoded,
  df_clusters,
) %>%
  mutate(
    adm0_pcode = "SDN",
    adm0_en = "Sudan",
    .before = adm1_pcode,
    pin = replace_na(pin, 0)
  )

# write_csv(
#   df_sdn,
#   file_paths$save_path
# )
