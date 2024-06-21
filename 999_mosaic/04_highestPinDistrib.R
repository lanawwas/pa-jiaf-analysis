# Install required packages if not already installed
if (!require("dplyr")) {
  install.packages("dplyr")
}
if (!require("tidyr")) {
  install.packages("tidyr")
}

# Load required libraries
library(dplyr)
library(tidyr)

# Read the CSV file
data <- read.csv("process/modified_data.csv")

# Step 1: Select the sector with the highest PiN for Admin.2(Lowest level of Analysis)
highest_pin_sectors <- data %>%
  group_by(Admin.2.Pcode) %>%
  filter(pin == max(pin)) %>%
  distinct(Admin.2.Pcode, .keep_all = TRUE)  # Keep all columns for the distinct rows

# Check the structure of the data
str(highest_pin_sectors)

# Step 2: Combine '1' and '2', and add them to '3_residual'
highest_pin_sectors_new <- highest_pin_sectors %>%
  group_by(Admin.2.Pcode) %>%
  mutate(
    across(ends_with("3"), ~ .x + coalesce(rowSums(across(ends_with(c("1", "2")))), 0), .names = "{.col}_residual")
  ) %>%
  #dplyr::select(-ends_with(c("1", "2"))) %>%
  ungroup()

# Step 3: Express the values stored in each severity level column as a percentage of the overall PiN
highest_pin_sectors_final <- highest_pin_sectors_new %>%
  mutate(
    across(contains("_"), ~ .x / pin * 100, .names = "{.col}_percentage")
  )

# Check the structure of the data
str(highest_pin_sectors_final)

# Save the updated data with percentages to a new CSV file
write.csv(highest_pin_sectors_final, "process/4.modified_data_highest_sector_pin.csv", row.names = FALSE)