library("ggplot2")
library("tidyverse")
library("scales")

# TODO: refactor filepaths to helpers
jiaf_dir <- Sys.getenv("JIAF_DATA_DIR")
save_path <- file.path(jiaf_dir, "Data analyzed")

# Plot the PINs
df_pins <- read.csv(
  file.path(
    save_path,
    "2022_hno_pin_totals.csv"
  )
) %>%
  group_by(adm0_pcode) %>%
  mutate(
    percent_diff = (pin - pin[sector_group == "intersectoral"])
    / pin[sector_group == "intersectoral"] * 100
  ) %>%
  ungroup() %>%
  mutate(
    sector_group = case_when(
      sector_group == "intersectoral" ~ "JIAF 1.1",
      sector_group == "sectoral" ~ "JIAF 2.0",
      sector_group == "sectoral_cluster" ~ "Cluster totals"
    ),
  )

p <- ggplot(df_pins, aes(
  fill = sector_group, y = pin, x = adm0_pcode,
  label = ifelse(percent_diff == 0, "",
    paste0(round(percent_diff, digits = 0), "%")
  )
)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(text = element_text(size = 7)) +
  geom_text(vjust = -0.5, position = position_dodge(width = 1), size = 1) +
  labs(
    fill = "Group",
    x = "Country ISO3",
    y = "PIN"
  ) +
  scale_y_continuous(label = comma)
ggsave(file.path(save_path, "2022_hno_pin_totals.png"))

# Plot the contribs

df_contrib <- read.csv(
  file.path(
    save_path,
    "2022_hno_pin_contributions.csv"
  )
) %>%
  mutate(
    max_pct = as.numeric(sub("%", "", max_pct)),
    pin_pct = as.numeric(sub("%", "", pin_pct))
  ) %>%
  arrange(adm0_pcode, desc(pin_pct))

ggplot(
  df_contrib %>% filter(sector_group == "sectoral", pin_pct > 0),
  aes(x = fct_inorder(max_sector), y = pin_pct, label = max_sector)
) +
  geom_bar(
    position = "dodge", stat = "identity", show.legend = FALSE,
    fill = "#FF6666"
  ) +
  theme(text = element_text(size = 4)) +
  coord_flip() +
  facet_grid(adm0_pcode ~ ., scales = "free", space = "free") +
  theme(axis.text.y = element_text(size = 2)) +
  labs(
    x = "sector",
    y = "% contribution to sectoral PIN"
  )
ggsave(file.path(save_path, "2022_hno_pin_contributions.png"))
