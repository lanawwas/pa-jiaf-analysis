library(tidyverse)
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

#########################
#### Prepare datasets ###
#########################

df_sectors <- read_csv(
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_cluster_totals.csv"
  )
) %>%
  mutate(
    across(
      .cols = matches("adm[1-3]"),
      .fns = tolower
    ),
    adm_name = case_when(
      adm0_pcode == "COL" ~ adm1_name,
      !is.na(adm3_name) ~ adm3_name,
      !is.na(adm2_name) ~ adm2_name,
      TRUE ~ adm1_name
    ),
    adm_name = gsub("[ ]|[-]|[.]|[_]|[,]", "", adm_name)
  ) %>%
  group_by(
    adm0_pcode,
    adm_name,
    sector
  ) %>%
  summarize(
    pin = sum(pin, na.rm = TRUE),
    affected_population = sum(affected_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    affected_population = max(round(affected_population))
  )

df_msna <- read_csv(
  file.path(
    dirname(getwd()),
    "MSNA_data.csv"
  )
) %>%
  mutate(
    across(
      .cols = protection_lsg:livelihoods_lsg,
      .fns = ~ case_when(
        . == "4+" ~ 5,
        is.na(.) ~ NA_real_,
        TRUE ~ as.numeric(.)
      )
    ),
    across(
      .cols = matches("admin[1-3]"),
      .fns = ~ tolower(iconv(.x, "UTF-8", "ASCII"))
    )
  ) %>%
  pivot_longer(
    cols = ends_with("_lsg"),
    names_to = "sector",
    values_to = "severity"
  ) %>%
  filter(
    !is.na(severity),
    admin0 %in% df_sectors$adm0_pcode
  ) %>%
  mutate(
    uuid,
    adm0_pcode = admin0,
    adm_name = case_when(
      !is.na(admin3_hno) ~ admin3_hno,
      admin2_hno == "baidoa" ~ "baydhaba",
      !is.na(admin2_hno) ~ admin2_hno,
      !is.na(admin1) ~ admin1
    ),
    sector = case_when(
      sector == "edu_lsg" ~ "Education",
      sector == "foodsec_lsg" ~ "FS/FSL",
      sector == "health_lsg" ~ "Health",
      sector == "markets_er_liv_lsg" ~ "ERL",
      sector == "protection_lsg" ~ "Protection",
      sector == "shelter_lsg" ~ "Shelter",
      sector == "wash_lsg" ~ "WASH"
    ),
    inneed = ifelse(severity >= 3, 1, 0),
    weight = as.numeric(weights)
  )

########################
### Analyse datasets ###
########################

df_max_sectors <- df_sectors %>%
  filter(adm0_pcode %in% unique(df_msna$adm0_pcode)) %>%
  group_by(
    adm0_pcode,
    adm_name,
    affected_population,
    .drop = TRUE
  ) %>%
  slice_max(order_by = pin, n = 2, with_ties = FALSE) %>%
  transmute(
    adm0_pcode,
    adm_name,
    affected_population,
    max_pin = pin[1],
    max_sector = sector[1],
    second_max_pin = pin[2],
    second_max_sector = sector[2]
  ) %>%
  unique()

df_msna_anlyse <- df_msna %>%
  left_join(
    df_max_sectors,
    by = c("adm0_pcode", "adm_name")
  ) %>%
  filter(!is.na(affected_population)) %>%
  mutate(
    inneed_max = ifelse(sector == max_sector &
      inneed == 1, 1, 0),
    inneed_second_max = ifelse(sector == second_max_sector &
      inneed == 1, 1, 0),
    inneed_other = ifelse(sector != max_sector &
      inneed == 1, 1, 0)
  ) %>%
  group_by(
    uuid,
    adm0_pcode,
    adm_name,
    max_sector,
    max_pin,
    second_max_sector,
    second_max_pin,
    affected_population,
    weight
  ) %>%
  summarize(
    inneed_max = sum(inneed_max),
    inneed_second_max = sum(inneed_second_max),
    inneed_other = sum(inneed_other),
    .groups = "drop"
  ) %>%
  mutate(
    inneed_second_max = ifelse(inneed_max > 0, 0, inneed_second_max),
    inneed_other = case_when(
      inneed_max > 0 ~ 0,
      inneed_other > 0 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(
    adm0_pcode,
    adm_name,
    max_sector,
    max_pin,
    second_max_sector,
    second_max_pin,
    affected_population
  ) %>%
  summarize(
    perc_second_max_nonoverlap = weighted.mean(inneed_second_max, weight),
    perc_all_other_sectors_nonoverlap = weighted.mean(inneed_other, weight),
    .groups = "drop"
  ) %>%
  mutate(
    pin_adj_second_max = max_pin +
      (affected_population * perc_second_max_nonoverlap),
    pin_adj_second_max = ifelse(
      pin_adj_second_max > affected_population,
      affected_population,
      pin_adj_second_max
    ),
    pin_adj_all_other = max_pin +
      (affected_population * perc_all_other_sectors_nonoverlap),
    pin_adj_all_other = ifelse(
      pin_adj_all_other > affected_population,
      affected_population,
      pin_adj_all_other
    )
  )

df_msna_anlyse %>%
  group_by(adm0_pcode) %>%
  summarise(
    max_pin = sum(round(max_pin)),
    pin_adj_all_other = sum(round(pin_adj_all_other)),
    pin_adj_second_max = sum(round(pin_adj_second_max)),
    affected_population = sum(round(affected_population))
  ) %>%
  pivot_longer(
    cols = -adm0_pcode,
    names_to = "pin_type"
  ) %>%
  mutate(
    pin_type = case_when(
      pin_type == "affected_population" ~ "Targeted population",
      pin_type == "max_pin" ~ paste0(
        "Max sectoral PiN",
        "(disaggregated only at admin level)"
      ),
      pin_type == "pin_adj_all_other" ~ paste0(
        "Max sectoral PiN",
        "(adjusted by non overlap needs with all other sectors)"
      ),
      pin_type == "pin_adj_second_max" ~ paste0(
        "Max sectoral PiN",
        "(adjusted by non overlap needs with second max sector)"
      )
    ),
    value = round(value / 1000000, 2)
  ) %>%
  arrange(adm0_pcode, desc(value)) %>%
  ggplot(aes(
    y = value,
    x = reorder(pin_type, +value),
    label = paste0(value, "M")
  )) +
  facet_wrap(~adm0_pcode,
    strip.position = "bottom",
    scales = "free_x"
  ) +
  geom_col(
    fill = "#1EBFB3"
  ) +
  geom_text(
    position = position_identity(),
    hjust = -.2,
    size = 4
  ) +
  coord_flip() +
  labs(
    x = "",
    y = "PIN",
    title = paste0(
      "Option 3: Comparison of HH severity aggregation, indicator PiN, ",
      "and sectoral PiN"
    )
  ) +
  theme_minimal() +
  scale_y_continuous(
    labels = function(x) {
      paste0(x, "M")
    },
    expand = expansion(c(0, .2))
  ) +
  scale_x_discrete(
    labels = function(x) {
      str_wrap(x, width = 35)
    }
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "2022_max_pin_adj_by_msna.png"
  ),
  bg = "transparent",
  height = 13,
  width = 25
)

write_csv(
  df_msna_anlyse,
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_max_pin_adj_by_msna.csv"
  )
)
