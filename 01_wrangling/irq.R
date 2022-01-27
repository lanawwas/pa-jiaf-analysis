library(tidyverse)
library(readxl)
library(janitor)

###################
#### DATA DIRS ####
###################

data_dir <- Sys.getenv("JIAF_DATA_DIR")

ocha_dir <- file.path(
  data_dir,
  "Data from country offices - OCHA",
  "Iraq"
)

cluster_dir <- file.path(
  data_dir,
  "Data from country offices - Clusters",
  "Iraq"
)

save_dir <- file.path(
  data_dir,
  "Data aggregated for analysis"
)

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  ocha_dir,
  "Iraq 2022 HNO Final Intersectoral & Cluster PIN Estimates - Updated 20211129.xlsx"
)

df_ocha_clusters_overall_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Overall-District"
) %>% mutate(population = "total")

df_ocha_clusters_in_camp_idps_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "In-Camp-IDPs-District"
) %>% mutate(population = "in-camp IDPs")

df_ocha_clusters_out_of_camp_idps_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Out-of-Camp-IDPs-District"
) %>% mutate(population = "out-of-camp IDPs")

df_ocha_clusters_returnees_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Returnees-District"
) %>% mutate(population = "returnees")

df_ocha_is_raw <- read_excel(
  ocha_fp, 
  skip = 3,
  sheet = "Gov. PIN & AcutePIN"
  )

########################
#### DATA WRANGLING ####
########################
# one code to bring them all

# saving pcodes and names to ensure unique names at end
df_pcodes <- df_ocha_clusters_overall_raw %>%
  select(
    adm1_en = admin1Pcode,
    adm2_en = admin2Pcode,
    gov_name,
    dist_name
  ) %>%
  distinct()

# Do a vertical join before pviot
df_ocha_clusters_raw = bind_rows(df_ocha_clusters_overall_raw, 
                                 df_ocha_clusters_in_camp_idps_raw, 
                                 df_ocha_clusters_out_of_camp_idps_raw, 
                                 df_ocha_clusters_returnees_raw)


# Pivot to make clusters and PINs  
df_ocha_clusters <- df_ocha_clusters_raw %>% 
  # Swap the prefix & suffix of the column names, so that pivot longer
  # works the right way
  rename_with( ~ gsub("(\\w+)_(pin|acute|sev)", "\\2_\\1", .x)) %>%
  pivot_longer(cols=starts_with("pin") | starts_with("acute") | starts_with("sev"), 
               names_to = c(".value", "cluster"),
               names_pattern="(\\w+)_(\\w+)")
  

# Pivoting the IS table
# Actually first need to fix the column names
df_ocha_is <- df_ocha_is_raw %>%
  pivot_longer()

# ocha provided cluster pins

names(df_ocha_clusters_raw) <- make_clean_names(names(df_ocha_clusters_raw))

df_ocha_clusters <- df_ocha_clusters_raw %>%
  select(
    sector,
    ends_with("pcode"),
    population_group,
    matches("male")
  ) %>%
  pivot_longer(
    matches("male"),
    names_to = c("sex", "age"),
    names_sep = "(?<=e)_",
    values_to = "pin"
  ) %>%
  mutate(
    source = "ocha",
    .before = 1
  )

# cluster provided pins

names(df_gbv_raw) <- make_clean_names(names(df_gbv_raw))

df_gbv <- df_gbv_raw %>%
  select(
    adm2_pcode = mantika_pcode,
    adm3_pcode = baladiya_p_code,
    population_group,
    ends_with("pi_n")
  ) %>%
  pivot_longer(
    ends_with("pi_n"),
    names_to = c("sex", "age"),
    names_pattern = "(.*)(?<=e)_(.*)_pi_n",
    values_to = "pin"
  ) %>%
  mutate(
    source = "cluster",
    sector = "GBV",
    .before = 1
  )

names(df_fs_raw) <- make_clean_names(names(df_fs_raw))

df_fs <- df_fs_raw %>%
  select(
    sector,
    ends_with("pcode"),
    population_group,
    matches("male")
  ) %>%
  pivot_longer(
    matches("male"),
    names_to = c("sex", "age"),
    names_sep = "(?<=e)_",
    values_to = "pin"
  ) %>%
  mutate(
    source = "cluster",
    .before = 1
  )

############################
#### GENERATE FULL DATA ####
############################
# and in the darkness bind them

df_lby <- bind_rows(
  df_ocha_clusters,
  df_ocha_is,
  df_fs,
  df_gbv
) %>%
  left_join(
    df_pcodes,
    by = c("adm2_pcode", "adm3_pcode")
  ) %>%
  select(
    source:adm2_pcode,
    adm2_en,
    adm3_pcode,
    adm3_en,
    population_group:pin
  ) %>%
  mutate(
    adm0_pcode = "LBY",
    adm0_en = "Libya",
    .before = adm2_pcode
  )

# write_csv(
#   df_lby,
#   file.path(
#     save_dir,
#     "lby_pins_2022.csv"
#   )
# )
