library(tidyverse)
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

###################
#### WRANGLING ####
###################

df_indicators <- read_csv(
  file.path(
    file_paths$agg_dir,
    "2022_indicator_pins.csv"
  )
)

df_prepped <- df_indicators %>%
  pivot_longer(
    matches("adm[1-3]+"),
    names_to = c(NA, "adm_level", NA),
    values_to = "adm_pcode",
    names_pattern = "(adm)([0-9]+)(_pcode)",
    names_transform = list(adm_level = as.numeric)
  ) %>%
  drop_na(adm_pcode) %>%
  group_by(
    adm0_name,
    adm0_pcode,
    indicator_number
  ) %>%
  slice_max(adm_level) %>%
  unite(pop_group,
    any_of(c(
      "population_group",
      "administration"
    )),
    sep = ", ",
    na.rm = TRUE
  ) %>%
  group_by(
    adm0_name,
    adm0_pcode,
    adm_pcode,
    pop_group
  )

################################
#### FUNCTION FOR SAMPLING #####
################################

ind_max <- function(df, indicators) {
  df %>%
    filter(
      indicator_number %in% !!indicators
    ) %>%
    summarize(
      pin = max(pin),
      .groups = "drop"
    ) %>%
    group_by(
      adm0_name
    ) %>%
    summarize(
      pin = sum(pin),
      n_indicators = !!length(indicators),
      .groups = "drop"
    )
}

sampler <- function(df) {
  # get number of unique indicators
  n <- max(df[["indicator_number"]])
  # group data frame for taking max
  df <- group_by(
    df,
    adm0_name,
    adm0_pcode,
    adm_pcode,
    pop_group
  )

  map_dfr(
    1:n,
    \(x) {
      map_dfr(
        1:200,
        \(y) {
          inds <- sample(1:n, x)
          ind_max(df, inds)
        }
      )
    }
  )
}

set.seed(123)

df_results <- df_prepped %>%
  group_by(
    adm0_pcode
  ) %>%
  group_split() %>%
  map_dfr(sampler)

# calculate PiN if just using critical indicators

df_crit_max <- df_prepped %>%
  filter(
    critical
  ) %>%
  summarize(
    pin = max(pin),
    n_indicators = length(unique(indicator_number)),
    .groups = "drop"
  ) %>%
  group_by(
    adm0_name
  ) %>%
  summarize(
    pin = sum(pin),
    n_indicators = unique(n_indicators),
    .groups = "drop"
  )

###############
#### GRAPH ####
###############

df_results %>%
  ggplot(
    aes(
      x = pin,
      y = n_indicators,
      group = n_indicators
    )
  ) +
  geom_boxplot() +
  geom_point(
    data = df_crit_max,
    size = 4,
    color = "#ef6666"
  ) +
  facet_wrap(
    ~adm0_name,
    scales = "free"
  ) +
  theme_minimal() +
  scale_x_continuous(
    labels = scales::comma
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks()
  ) +
  geom_label(
    data = data.frame(
      adm0_name = "Cameroon",
      pin = 3000000,
      n_indicators = 2
    ),
    label = "Critical\nindicators PiN",
    hjust = 0,
    size = 3,
    fill = "#ef6666",
    color = "white"
  ) +
  geom_segment(
    data = data.frame(
      adm0_name = "Cameroon",
      pin = 2780000,
      n_indicators = 2.8
    ),
    xend = 3100000,
    yend = 1.8,
    lwd = 1,
    color = "#ef6666"
  ) +
  labs(
    title = "Distribution of PiNs based on number of indicators",
    subtitle = "PiNs calculated from max of sectoral indicator PiNs",
    y = "Number of indicators",
    x = "PiN",
    caption = "Critical indicators PiN calculated from all critical indicators"
  ) +
  theme(
    plot.background = element_rect(fill = "white")
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "2022_sectoral_pin_sampling_plot.png"
  ),
  height = 10,
  width = 12
)
