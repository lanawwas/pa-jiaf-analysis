library(tidyverse)
library(readxl)

###################
#### DATA DIRS ####
###################

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths("Nigeria")

############################
#### OCHA PROVIDED DATA ####
############################

fp_ocha <- file.path(
  file_paths$ocha_dir,
  "Nigeria HPC 2022 projected_PIN_All_Data_Shared.xlsx"
)

df_ocha_raw <- read_excel(
  fp_ocha,
  sheet = "PiN_LGA_Sectors",
  range = ("A2:R67"),
) %>% mutate(
  LGA = LGA %>% str_replace("Abandam", "Abadam")
)

df_ocha_clusters <- df_ocha_raw %>%
  select(adm2_en= LGA, WASH:`PRO-HLP`) %>%
  pivot_longer(
    cols = WASH:`PRO-HLP`,
    names_to = "sector",
    values_to = "pin"
  )

df_ocha_is <- df_ocha_raw %>%
  select(
    adm2_en = LGA,
    pin = `Estimated JIAF PIN\r\n(Severity classes 3, 4 & 5)`
  ) %>%
  mutate(sector = "intersectoral")

df_ocha <- bind_rows(
  df_ocha_clusters,
  df_ocha_is
) %>%
  mutate(source = "ocha", .before = 1)


################
#### PCODES ####
################

df_pcodes <- read_excel(
  fp_ocha,
  sheet = "data",
  skip = 1
) %>%
  select(
    adm1_pcode = `state Pcode`,
    adm1_en = State,
    adm2_pcode = `LGA pcode`,
    adm2_en = LGA
  ) %>%
  unique()

######################
#### CLUSTER DATA ####
######################

# Shelter & NFIs
df_shelter = read_excel(
  file.path(
    file_paths$cluster_dir,
    "Nigeria_2022 ESNFI PiN & Target_01122021.xlsx"
  ),
  sheet = "PiN Breakdown Type"
) %>% 
  select(
    adm2_en = LGA,
    PIN = `Total Population`
  ) %>% drop_na() %>%
mutate(sector = "Shelter & NFIs")

# Combine clusters
df_clusters <- df_shelter %>% 
  mutate(source = "cluster")


############################
#### GENERATE FULL DATA ####
############################

df_nga <- bind_rows(
  df_ocha,
  df_clusters)%>%
  left_join(df_pcodes,
    by = "adm2_en",
  ) %>%
  relocate(adm1_pcode:adm2_pcode, .before = adm2_en) %>%
  drop_na("adm1_en") %>%
  mutate(
    adm0_en = "Nigeria",
    adm0_pcode = "nga",
    .before = adm1_pcode,
  )

# write_csv(
#   df_nga,
#   file_paths$save_path
# )
