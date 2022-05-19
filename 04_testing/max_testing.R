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
  grouped_df <- df %>%
    group_by(
      across(-c(sector, pin))
    ) %>%
    summarize(
      pin = max(pin),
      .groups = "drop_last"
    )

  # only if we have enough groups
  if (ncol(grouped_df) > 2) {
    grouped_df <- grouped_df %>%
      summarize(
        pin = sum(pin),
        agg_groups_n = n(),
        agg_groups_name = names(.)[ncol(.) - 1],
        .groups = "drop"
      ) %>%
      group_by(
        adm0_pcode
      ) %>%
      summarize(
        agg_cols = paste(
          c(names(.)[1:(ncol(.) - 3)], unique(agg_groups_name)),
          collapse = ", "
        ),
        agg_cols_n = ncol(.) - 3,
        agg_groups_n = mean(agg_groups_n),
        agg_groups_name = unique(agg_groups_name),
        pin = sum(pin),
        .groups = "drop"
      )
  } else {
    grouped_df <- grouped_df %>%
      group_by(
        adm0_pcode
      ) %>%
      summarize(
        agg_cols = paste(
          names(.)[1],
          collapse = ", "
        ),
        agg_cols_n = ncol(.) - 2,
        pin = sum(pin),
        .groups = "drop"
      )
  }

  grouped_df
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
    \(x) {
      cols <- c(names(df)[1:x], "sector")
      df %>%
        group_by(
          across(cols)
        ) %>%
        summarize(
          pin = sum(pin),
          .groups = "drop"
        ) %>%
        max_pin()
    }
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

# check PiN for level of disaggregation

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

# difference between highest a lowest aggregation

results_df %>%
  group_by(
    adm0_pcode
  ) %>%
  filter(
    agg_cols_n %in% c(max(agg_cols_n), min(agg_cols_n))
  )
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


# see drop in % based on # of agg groups
results_df %>%
  group_by(
    adm0_pcode
  ) %>%
  mutate(
    pin_change = (pin - lag(pin)) / lag(pin),
    agg_groups_n = lag(agg_groups_n),
    agg_groups_name = lag(agg_groups_name),
    agg_groups_type = case_when(
      str_starts(agg_groups_name, "adm") ~ "Admin",
      agg_groups_name == "population_group" ~ "Population group",
      TRUE ~ str_to_title(agg_groups_name)
    )
  ) %>%
  filter(
    !is.na(agg_groups_n)
  ) %>%
  ggplot(
    aes(
      y = pin_change,
      x = agg_groups_n,
      color = agg_groups_type
    )
  ) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_brewer(
    palette = "PuOr"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    y = "% reduction in PiN when aggregation removed",
    x = "Average # of unique groups in aggregation",
    color = "Type of aggregation",
    title = "Impact of aggregation on max PiN"
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "2022_hno_max_test_pct.png"
  ),
  height = 6,
  width = 5
)
