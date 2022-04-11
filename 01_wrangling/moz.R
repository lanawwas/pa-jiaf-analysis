library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Mozambique")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "MOZ_HNO2022_INTERSECTORAL_PIN_&_TARGET_20211116-v11_final .xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Combined_Intersectoral_PIN"
) %>%
  clean_names() %>%
  drop_na(adm1_province)

########################
#### DATA WRANGLING ####
########################

df_cleaned <- df_ocha_raw %>%
  mutate(
    fsc = as.numeric(number_of_people_in_ipc_phases),
    nutrition = as.numeric(number_children_6_59_months_with_global_acute_malnutrition_gam_based_on_weight_for_height_z_score_whz_2_and_or_bilateral_pitting_oedema),
    protection = as.numeric(indicator_6_7_indicator_6_availability_of_core_gbv_services_gbv_case_management_individual_psychosocial_support_pss_clinical_management_of_rape_cmr_medical_services_for_ipv_other_physical_violence_mental_health_indicator_7_number_of_gbv_risk_factors_per_location),
    shelter = as.numeric(number_of_individuals_currently_living_in_unsustainable_shelter_situations),
    wash = as.numeric(number_of_people_not_accessing_a_sufficient_quantity_of_safe_water_for_drinking),
    intersectoral = as.numeric(overall_max)
  ) %>%
  pivot_longer(
    cols = fsc:intersectoral,
    names_to = "sector",
    values_to = "pin"
  ) %>%
  transmute(
    adm0_name = "Mozambique",
    adm0_pcode = "MOZ",
    adm1_name = adm1_province,
    adm1_pcode = adm1_pcode,
    adm2_name = adm2_district,
    adm2_pcode = adm2_pcode,
    population_group = "total",
    sector,
    pin,
    source = "ocha",
    sector_general = ifelse(sector == "intersectoral", "intersectoral", "sectoral")
  )

write_csv(
  df_cleaned,
  file_paths$save_path
)
