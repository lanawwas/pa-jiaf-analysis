library(tidyverse)

source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

######################
#### READING DATA ####
######################

df <- read_csv(
  file.path(
    file_paths$output_dir_sev,
    "datasets",
    "sectoral_severity_metrics.csv"
  )
)

##########################################
#### DIFFERENT INTERSECTORAL SEVERITY ####
##########################################

# no graph saved as no distinct differences

df %>%
  select(
    adm0_name,
    disaggregation,
    ERL:`Protection (HLP)`,
    intersectoral_severity
  ) %>%
  distinct() %>%
  group_by(
    adm0_name,
    disaggregation,
    across(
      ERL:`Protection (HLP)`
    )
  ) %>%
  summarize(
    n_intersectoral_severity = n(),
    .groups = "drop"
  )

df_is_relationship <- df %>%
  filter(
    !is.na(intersectoral_severity)
  ) %>%
  mutate(
    mean_cut = cut(
      mean_severity,
      breaks = seq(0, 5, 0.5)
    ),
    perc_sectors_cut = cut(
      perc_sectors_3_over,
      breaks = seq(0, 1, 0.1),
      include.lowest = TRUE
    )
  ) %>%
  group_by(
    mean_cut,
    perc_sectors_cut
  ) %>%
  summarize(
    intersectoral_severity = mean(intersectoral_severity, na.rm = TRUE)
  )

df_is_relationship %>%
  ggplot(
    aes(
      x = mean_cut,
      y = perc_sectors_cut,
      fill = intersectoral_severity
    )
  ) +
  geom_tile(
    color = "white"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#1EBFB3"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = .5,
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text.x = element_text(
      face = "bold",
      margin = margin(20, 0, 20, 0, "pt"),
      size = 10,
      family = "Roboto"
    ),
    axis.text.y = element_text(
      face = "bold",
      margin = margin(0, 20, 0, 20, "pt"),
      size = 10,
      family = "Roboto"
    ),
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  ) +
  scale_x_discrete(
    labels = paste(
      seq(0.5, 4.5, 0.5),
      seq(1, 5, 0.5),
      sep = " - "
    )
  ) +
  scale_y_discrete(
    labels = paste(
      paste0(100 * seq(0, 0.9, 0.1), "%"),
      paste0(100 * seq(0.1, 1, 0.1), "%"),
      sep = " - "
    )
  ) +
  labs(
    y = "% of sectors with severity 3+",
    x = "Mean of sectoral severities",
    fill = "Intersectoral severity\n(mean)",
    title = paste(
      "Intersectoral severity, by % of sectors with high severity",
      "and mean sectoral severity"
    )
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_perc_3_mean_is_relationship.png"
  ),
  height = 7,
  width = 12,
  units = "in"
)

write_csv(
  df_is_relationship,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_perc_3_mean_is_relationship.csv"
  )
)
