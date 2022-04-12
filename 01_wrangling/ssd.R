library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("South Sudan")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "SS_20220127_HNO_2022_JIAF1.1_AggregationTemplate_Scenario-B.xlsx"
)

df_ocha_raw <- read_excel(
    ocha_fp,
    skip = 1,
    sheet = "SADD based on clusters data"
  ) %>%
    clean_names()

########################
#### DATA WRANGLING ####
########################

# the column names are random, reorganizing the names
df_ocha <- df_ocha_raw %>%
  rename(
    adm1_name = x1,
    adm1_pcode = x2,
    adm2_name = x3,
    adm2_pcode = x4,
    intersectoral_male_child = male_child,
    intersectoral_female_child = female_child_0_5_7,
    intersectoral_male_adult = adult_male_18_59_8,
    intersectoral_female_adult = adult_female_18_59_9,
    intersectoral_total_total = total_pi_n,
    cccm_male_child.0.5 = male_child_0_5,
    cccm_female_child.0.5 = female_child_0_5_14,
    cccm_male_child.6.17 = male_6_17,
    cccm_female_child.6.17 = female_6_17,
    cccm_male_adult.18.59 = adult_male_18_59_17,
    cccm_female_adult.18.59 = adult_female_18_59_18,
    cccm_male_elderly.60 = elderly_male_60,
    cccm_female_elderly.60 = elderly_female_60,
    education_male_child = ci_n_boys,
    education_female_child = ci_n_girls,
    education_male_adult = adult_male,
    education_female_adult = adult_female,
    fsl_male_child.0.4 = no_of_male_children_under_5_25,
    fsl_female_child.0.4 = no_of_female_children_under_5_26,
    fsl_male_child.5.17 = no_of_male_children_aged_5_17_years_27,
    fsl_female_child.5.17 = no_of_female_children_aged_5_17_years_28,
    fsl_male_adult.18.60 = no_of_male_adults_aged_18_60_29,
    fsl_female_adult.18.60 = no_of_female_adults_aged_18_60_30,
    fsl_male_elderly.61 = no_of_male_adults_aged_over_60_31,
    fsl_female_elderly.61 = no_female_adults_aged_over_60_32,
    health_male_child.0.4 = no_of_male_children_under_5_33,
    health_female_child.0.4 = no_of_female_children_under_5_34,
    health_male_child.5.17 = no_of_male_children_aged_5_17_years_35,
    health_female_child.5.17 = no_of_female_children_aged_5_17_years_36,
    health_male_adult.18.60 = no_of_male_adults_aged_18_60_37,
    health_female_adult.18.60 = no_of_female_adults_aged_18_60_38,
    health_male_elderly.61 = no_of_male_adults_aged_over_60_39,
    health_female_elderly.61 = no_female_adults_aged_over_60_40,
    nutrition_male_child = hc_cu5_boys,
    nutrition_female_child = hc_cu5_girls,
    nutrition_male_adult = x43,
    nutrition_plw_plw = hc_plw,
    protection_male_child = pc_boys,
    protection_female_child = pc_girls,
    protection_male_adult = pc_men,
    protection_female_adult = pc_women,
    snfi_male_child.0.4 = number_of_male_children_under_5,
    snfi_female_child.0.4 = number_of_female_children_under_5,
    snfi_male_child.5.17 = number_of_male_children_aged_5_17_years,
    snfi_female_child.5.17 = number_of_female_children_aged_5_17_years,
    snfi_male_adult.18.60 = number_of_male_adults_aged_18_60,
    snfi_female_adult.18.60 = number_of_female_adults_aged_18_60,
    snfi_male_elderly.61 = number_of_male_adults_aged_over_60,
    snfi_female_elderly.61 = number_of_female_adults_aged_over_60,
    wash_male_child = boys,
    wash_female_child = girls,
    wash_male_adult = men,
    wash_female_adult = women
  ) %>%
  remove_empty("cols") %>%
  pivot_longer(
    cols = c(intersectoral_male_child:wash_female_adult, -severity, -pi_n_population),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  separate(sector, c("sector", "sex", "age"), sep = "_") %>%
  transmute(
    adm0_name = "South Sudan",
    adm0_pcode = "SSD",
    adm1_name,
    adm1_pcode,
    adm2_name,
    adm2_pcode,
    sector,
    sex,
    age,
    population_group = "total",
    pin = round(pin),
    source = "ocha",
    sector_general = ifelse(sector == "intersectoral", "intersectoral", "sectoral")
  )

write_csv(
  df_ocha,
  file_paths$save_path
)



