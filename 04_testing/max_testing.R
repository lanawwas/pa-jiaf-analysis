library(tidyverse)
library(janitor)
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

#################
#### LOADING ####
#################

# load data with only aggregate columns + sector + pin
df <- read_csv(
  file.path(
    file_paths$output_dir,
    "2022_hno_pin_cluster_totals.csv"
  )
) %>%
  select(
    ends_with("pcode"),
    administration,
    population_group,
    sex,
    age,
    sector,
    pin
  )

###################
#### FUNCTIONS ####
###################

# calculates the max pin
# based on available data
max_pin <- function(df) {
  df %>%
    group_by(
      across(-c(sector, pin))
    ) %>%
    summarize(
      pin = max(pin),
      .groups = "drop"
    ) %>%
    group_by(
      adm0_pcode
    ) %>%
    summarize(
      agg_cols = paste(
        names(.)[1:(ncol(.) - 1)],
        collapse = ", "
      ),
      agg_cols_n = ncol(.) - 2,
      pin = sum(pin)
    )
}

# tests all combinations for specific adm0
tester <- function(df) {
  # remove aggregation columns with no data
  df <- remove_empty(df, which = "cols")

  # for all agg columns, calculate max pin for those columns
  # then drop last column and calculate for next column
  n <- ncol(df)
  map_dfr(
    (n - 2):1,
    ~ max_pin(df[, c(1:.x, n - 1, n)])
  )
}

#################
#### TESTING ####
#################

results_df <- df %>%
  group_by(adm0_pcode) %>%
  group_split() %>%
  map_dfr(tester)

results_df %>%
  write_csv(
    file.path(
      file_paths$output_dir,
      "2022_hno_max_test.csv"
    )
  )

################
#### VISUAL ####
################

results_df %>%
  group_by(
    adm0_pcode
  ) %>%
  mutate(
    pin = pin / pin[1]
  ) %>%
  filter(agg_cols_n != 0) %>%
  ggplot(
    aes(
      y = pin,
      x = agg_cols_n,
      fill = agg_cols_n
    )
  ) +
  geom_bar(stat = "identity") +
  facet_grid(
    adm0_pcode ~ "",
    scales = "free_y"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "white")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    y = "PiN (% of max PiN for most disaggregated data)",
    fill = "Number of\ndisaggregations",
    title = "Comparison of PiNs by level of disaggregation",
    caption = paste0(
      "Disaggregations go 1 to 6 from administrative ",
      "boundaries to population groups to sex and age"
    )
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "2022_hno_max_test.png"
  ),
  height = 13,
  width = 8
)
