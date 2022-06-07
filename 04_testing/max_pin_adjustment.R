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
  mutate_at(
    .vars = vars(matches("adm[1-3]")),
    .funs = tolower
  ) %>%
  mutate(
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
    affected_population = sum(affected_population, na.rm = TRUE)
  ) %>%
  mutate(
    affected_population =
      ifelse(affected_population < pin, pin, affected_population)
  )

df_msna <- read.csv(file.path("../MSNA_data.csv")) %>%
  mutate_at(
    .vars = vars(protection_lsg:livelihoods_lsg),
    .funs = as.numeric
  ) %>%
  mutate(
    admin1 = gsub("[ ]|[-]|[.]|[_]|[,]", "", tolower(admin1)),
    admin2_hno = gsub("[ ]|[-]|[.]|[_]|[,]", "", tolower(admin2_hno)),
    admin3_hno = gsub("[ ]|[-]|[.]|[_]|[,]", "", tolower(admin3_hno))
  ) %>%
  pivot_longer(
    cols = matches("_lsg"),
    names_to = "sector",
    values_to = "severity"
  ) %>%
  filter(
    !is.na(severity),
    admin0 %in% df_sectors$adm0_pcode
  ) %>%
  transmute(
    uuid,
    adm0_pcode = admin0,
    adm_name = case_when(
      !is.na(admin3_hno) &
        admin3_hno %in% unique(df_sectors$adm_name) ~ admin3_hno,
      !is.na(admin2_hno) &
        admin2_hno %in% unique(df_sectors$adm_name) ~ admin2_hno,
      admin2_hno == "baidoa" ~ "baydhaba",
      !is.na(admin1) & admin1 %in% unique(df_sectors$adm_name) ~ admin1
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
    .drop = TRUE
  ) %>%
  slice_max(order_by = pin, n = 2, with_ties = FALSE) %>%
  transmute(
    adm0_pcode,
    adm_name,
    affected_population = affected_population[1],
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
    inneed_second_max = weighted.mean(inneed_second_max, w = weight),
    inneed_other = weighted.mean(inneed_other, w = weight),
    .groups = "drop"
  ) %>%
  mutate(
    pin_adj_second_max = max_pin + (affected_population * inneed_second_max),
    pin_adj_second_max = ifelse(
      pin_adj_second_max > affected_population,
      affected_population,
      pin_adj_second_max
    ),
    pin_adj_all_other = max_pin + (affected_population * inneed_other),
    pin_adj_all_other = ifelse(
      pin_adj_all_other > affected_population,
      affected_population,
      pin_adj_all_other
    ),
  ) %>%
  group_by(
    adm0_pcode
  ) %>%
  summarise(
    max_pin = sum(round(max_pin)),
    pin_adj_all_other = sum(round(pin_adj_all_other)),
    pin_adj_second_max = sum(round(pin_adj_second_max)),
    affected_population = sum(round(affected_population))
  ) %>%
  pivot_longer(
    cols = -adm0_pcode,
    names_to = "aggregation_method"
  ) %>%
  mutate(
    aggregation_method = case_when(
      aggregation_method == "affected_population" ~ "Targeted population",
      aggregation_method == "max_pin" ~ paste0(
        "Max sectoral PiN",
        "(disaggregated only at admin level)"
      ),
      aggregation_method == "pin_adj_all_other" ~ paste0(
        "Max sectoral PiN",
        "(adjusted by non overlap needs with all other sectors)"
      ),
      aggregation_method == "pin_adj_second_max" ~ paste0(
        "Max sectoral PiN",
        "(adjusted by non overlap needs with second max sector)"
      )
    ),
    value = round(value / 1000000, 2)
  ) %>%
  arrange(adm0_pcode, desc(value))

ggplot(
  df_msna_anlyse,
  aes(
    y = value,
    x = reorder(aggregation_method, +value),
    label = paste0(value, "M")
  )
) +
  facet_wrap(
    ~adm0_pcode,
    strip.position = "bottom",
    scales = "free_x"
  ) +
  geom_col() +
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
  theme_light() +
  scale_y_continuous(
    labels = function(x) paste0(x, "M"),
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
      margin = margin(10, 10, 30, 10, "pt"),
      family = "Roboto"
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
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent"),
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
    "2022_hh_data_aggregations.png"
  ),
  bg = "transparent",
  height = 13,
  width = 25,
  plot = bar_chart
)
