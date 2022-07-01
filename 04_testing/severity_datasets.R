library(tidyverse)

source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

###################
#### Reading Data ####
###################

df <- read_csv(
  file.path(
    file_paths$agg_dir,
    "2022_sectoral_sev.csv"
  )
) %>%
  unite(
    disaggregation,
    any_of(
      c(
        "adm1_pcode",
        "adm2_pcode",
        "adm3_pcode",
        "population_group",
        "administration"
      )
    ),
    sep = ", ",
    na.rm = TRUE
  )

########################
#### OUTPUT DATASET ####
########################

df_stats <- df %>%
  filter(
    sector_general == "sectoral"
  ) %>%
  left_join(
    df %>%
      filter(
        sector == "Intersectoral"
      ) %>%
      select(
        disaggregation,
        intersectoral = severity
      ),
    by = "disaggregation"
  ) %>%
  group_by(
    adm0_name,
    adm0_pcode,
    disaggregation
  ) %>%
  summarize(
    mean_severity = mean(severity),
    sum_severity = sum(severity),
    n_sectors_2_over = sum(severity >= 2),
    n_sectors_3_over = sum(severity >= 3),
    n_sectors_4_over = sum(severity >= 4),
    n_sectors_5_over = sum(severity >= 5),
    perc_sectors_2_over = n_sectors_2_over / n(),
    perc_sectors_3_over = n_sectors_3_over / n(),
    perc_sectors_4_over = n_sectors_4_over / n(),
    perc_sectors_5_over = n_sectors_5_over / n(),
    n_sectors = n(),
    intersectoral_severity = unique(intersectoral),
    .groups = "drop"
  )


df_sectors <- df %>%
  filter(
    sector_general == "sectoral"
  ) %>%
  select(
    disaggregation,
    sector,
    severity
  ) %>%
  pivot_wider(
    names_from = sector,
    values_from = severity
  )

full_join(
  df_stats,
  df_sectors,
  by = "disaggregation"
) %>%
  write_csv(
    file.path(
      file_paths$output_dir_sev,
      "datasets",
      "sectoral_severity_metrics.csv"
    )
  )
