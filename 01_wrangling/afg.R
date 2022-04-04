library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Afghanistan")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "afg_hno_pin_2022_Clusters.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Total"
) %>%
  clean_names() %>%
  mutate(
    sector = "intersectoral"
  ) %>%
  rename(number_admin1_code = number_adm1_code,
         number_admin1_name = number_adm1_name,
         number_sector_name = number_sector)

df_edu_raw <- read_excel(
  ocha_fp,
  skip = 5,
  sheet = "EDU"
) %>%
  clean_names() %>% 
  mutate(sector = "education")

df_shl_raw <- read_excel(
  ocha_fp,
  skip = 5,
  sheet = "SHL"
) %>%
  clean_names() %>% 
  mutate(sector = "shelter")

df_fsa_raw <- read_excel(
  ocha_fp,
  skip = 5,
  sheet = "FSA"
) %>%
  clean_names() %>% 
  mutate(sector = "food security")

df_health_raw <- read_excel(
  ocha_fp,
  skip = 5,
  sheet = "HEA"
) %>%
  clean_names() %>% 
  mutate(sector = "health")

df_nutr_raw <- read_excel(
  ocha_fp,
  skip = 5,
  sheet = "NUT"
) %>%
  clean_names() %>% 
  mutate(sector = "nutrition")

df_prot_raw <- read_excel(
  ocha_fp,
  skip = 5,
  sheet = "PRO"
) %>%
  clean_names() %>% 
  mutate(sector = "protection")

df_wash_raw <- read_excel(
  ocha_fp,
  skip = 5,
  sheet = "WSH"
) %>%
  clean_names() %>% 
  mutate(sector = "wash")
  

########################
#### DATA WRANGLING ####
########################

# one code to bring them all
df_combined_all <- bind_rows(
  df_ocha_raw,
  df_edu_raw,
  df_shl_raw,
  df_fsa_raw,
  df_health_raw,
  df_nutr_raw,
  df_prot_raw,
  df_wash_raw
)

# dropping unnecessary columns
df_combined_all <- df_combined_all[,c(1:4, 10:34, 40)]
names(df_combined_all) <- gsub("_x_", "_", names(df_combined_all))

df_combined_all <- df_combined_all %>%
  pivot_longer(
    cols = matches("^number_inneed"),
    names_to = c(".value", "group"),
    names_pattern = "(^number_inneed)_(.*)"
  ) %>%
  mutate(
    population_group = gsub("(^m_children_|^f_children_|^m_adult_|^f_adult_|^total_)", "", group)
  ) %>%
  filter(!grepl("total", group))

############################
#### GENERATE FULL DATA ####
############################

# calculating total pin
df_total <- df_combined_all %>%
  group_by(
    number_admin1_code,
    number_admin1_name,
    population_group,
    sector
    ) %>%
  summarise(
    pin = sum(number_inneed, na.rm = T)
    ) %>%
  mutate(
    adm0_pcode = "AFG",
    adm0_en = "Afghanistan",
    source = "ocha",
    sectoral_general = ifelse(sector == "intersectoral", "intersectoral", "sectoral")
  ) 

df_total <- df_total%>%
  select(    
    adm0_pcode,
    adm0_en,
    adm1_pcode = number_admin1_code,
    adm1_en = number_admin1_name,
    population_group,
    sector,
    pin,
    source,
    sectoral_general
  )

write_csv(
  df_total,
  file_paths$save_path
)
