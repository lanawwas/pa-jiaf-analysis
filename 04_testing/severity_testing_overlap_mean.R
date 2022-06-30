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

###################################
#### SAME GRAPH, ALL COUNTRIES ####
###################################

df_is_relationship_country <- df %>%
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
    adm0_pcode,
    mean_cut,
    perc_sectors_cut
  ) %>%
  summarize(
    intersectoral_severity = mean(intersectoral_severity, na.rm = TRUE)
  )

df_is_relationship_country %>%
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
      family = "Roboto",
      angle = -90
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
  facet_wrap(
    ~adm0_pcode
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
    "2022_perc_3_mean_is_relationship_country.png"
  ),
  height = 10,
  width = 15,
  units = "in"
)

write_csv(
  df_is_relationship_country,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_perc_3_mean_is_relationship_country.csv"
  )
)

#####################################
#### COMPARE MEAN TO IS SEVERITY ####
#####################################

df_mean <- df %>%
  mutate(
    sev_diff = mean_severity - intersectoral_severity
  ) %>%
  filter(
    !(adm0_pcode %in% c("LBY", "MLI", "VEN"))
  )

df_mean %>%
  ggplot() +
  geom_rect(
    data = data.frame(
      xmin = c(-3, 0),
      xmax = c(0, 3),
      ymin = 0,
      fill = c("#F2645A", "#007CE0")
    ),
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      fill = fill
    ),
    ymax = Inf,
    alpha = 0.25
  ) +
  geom_histogram(
    aes(
      x = sev_diff
    ),
    fill = "#1EBFB3",
    binwidth = function(x) 2 * IQR(x) / (length(x)^(1 / 3))
  ) +
  facet_wrap(
    ~adm0_pcode,
    scales = "free"
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
  scale_fill_identity() +
  labs(
    y = "# of geographical areas",
    x = "Sectoral severity (mean) - intersectoral severity",
    title = paste(
      "Difference, mean sectoral severity and intersectoral",
      "severity, by country"
    )
  ) +
  geom_label(
    data = data.frame(
      x = c(-1.9, 1.9),
      y = 150,
      adm0_pcode = "COL",
      label = c("Mean < IS", "Mean > IS")
    ),
    aes(
      x = x,
      y = y,
      label = label
    ),
    family = "Roboto",
    color = "white",
    fill = "#888888",
    label.size = 0,
    size = 2.5
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_diff_mean_intersectoral_country.png"
  ),
  height = 7,
  width = 11,
  units = "in"
)

df_mean %>%
  ggplot() +
  geom_rect(
    data = data.frame(
      xmin = c(-3, 0),
      xmax = c(0, 3),
      ymin = 0,
      fill = c("#F2645A", "#007CE0")
    ),
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      fill = fill
    ),
    ymax = Inf,
    alpha = 0.25
  ) +
  geom_histogram(
    aes(
      x = sev_diff
    ),
    fill = "#1EBFB3",
    binwidth = function(x) 2 * IQR(x) / (length(x)^(1 / 3))
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
  scale_fill_identity() +
  labs(
    y = "# of geographical areas",
    x = "Sectoral severity (mean) - intersectoral severity",
    title =
      "Difference, mean sectoral severity and intersectoral severity"
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_diff_mean_intersectoral.png"
  ),
  height = 5,
  width = 10,
  units = "in"
)

write_csv(
  df_mean,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_diff_mean_intersectoral.csv"
  )
)

###################################################
#### LOOK AT OVERLAPPING NEEDS VS IS SEVERITY #####
###################################################

df %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = intersectoral_severity,
      y = perc_sectors_3_over,
      group = intersectoral_severity
    ),
    color = "#1EBFB3"
  ) +
  labs(
    y = "% of sectors in severity 3 or above",
    x = "Intersectoral severity",
    title =
      "Overlapping severity > 3 and intersectoral severity"
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
  scale_y_continuous(
    labels = scales::percent_format(1)
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_overlap_intersectoral.png"
  ),
  height = 5,
  width = 9,
  units = "in"
)


df %>%
  filter(
    !(adm0_pcode %in% c("LBY", "MLI", "VEN"))
  ) %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = intersectoral_severity,
      y = perc_sectors_3_over,
      group = intersectoral_severity
    ),
    color = "#1EBFB3"
  ) +
  labs(
    y = "% of sectors in severity 3 or above",
    x = "Intersectoral severity",
    title =
      "Overlapping severity > 3 and intersectoral severity, by country"
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
  scale_y_continuous(
    labels = scales::percent_format(1)
  ) +
  facet_wrap(
    ~adm0_pcode
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_overlap_intersectoral_country.png"
  ),
  height = 10,
  width = 15,
  units = "in"
)

##########################
#### LOOK AT OUTLIERS ####
##########################

df_sd <- df %>%
  pivot_longer(
    ERL:`Protection (HLP)`,
    names_to = "sector",
    values_to = "severity"
  ) %>%
  group_by(
    adm0_pcode,
    disaggregation
  ) %>%
  summarize(
    severity_sd = sd(severity, na.rm = TRUE),
    outlier_1sd = any(
      abs(severity - mean_severity) > severity_sd,
      na.rm = TRUE
    ),
    outlier_2sd = any(
      abs(severity - mean_severity) > 2 * severity_sd,
      na.rm = TRUE
    ),
    outlier_1sd_pos = any(
      (severity - mean_severity > severity_sd),
      na.rm = TRUE
    ),
    outlier_2sd_pos = any(
      (severity - mean_severity > 2 * severity_sd),
      na.rm = TRUE
    ),
    mean_severity = unique(mean_severity),
    perc_sectors_3_over = unique(perc_sectors_3_over),
    .groups = "drop"
  )

df_sd %>%
  ggplot(
    aes(
      x = mean_severity,
      y = perc_sectors_3_over
    )
  ) +
  geom_point(
    color = "#78D9D1",
    alpha = 0.1
  ) +
  geom_point(
    data = filter(df_sd, outlier_2sd_pos),
    color = "#F2645A"
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
  scale_y_continuous(
    labels = scales::percent_format(1)
  ) +
  geom_line(
    data = data.frame(
      mean_severity = c(1.25, 1.6),
      perc_sectors_3_over = .223
    ),
    arrow = arrow(angle = 20),
    color = "#F2645A"
  ) +
  geom_text(
    data = data.frame(
      mean_severity = 1.05,
      perc_sectors_3_over = .225
    ),
    label = "Outliers",
    fontface = "bold",
    family = "Roboto",
    color = "#F2645A"
  ) +
  labs(
    x = "Mean of sectoral severities",
    y = "% of sectors with severity 3+",
    title = "Areas with outliers, comparing mean and overlapping severity",
    caption = paste(
      "Only positive outliers plotted, those where sectoral severity is",
      "over 2 standard deviations above the mean."
    )
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_mean_overlap_outliers.png"
  ),
  height = 8,
  width = 12,
  units = "in"
)

write_csv(
  df_sd,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_mean_overlap_outliers.csv"
  )
)

############################################
#### LOOKING ALSO AT STANDARD DEVIATION ####
############################################

df_sd_cut <- df_sd %>%
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
    adm0_pcode,
    mean_cut,
    perc_sectors_cut
  ) %>%
  summarize(
    mean_sd = mean(severity_sd, na.rm = TRUE),
    .groups = "drop"
  )

df_sd_cut %>%
  ggplot(
    aes(
      x = mean_cut,
      y = perc_sectors_cut,
      fill = mean_sd
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
    fill = "Standard deviation of\nsectoral severity",
    title = "Standard deviation of sectoral severity, by mean and overlap"
  )
