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

# Step 1: Calculate overall PiN for each admin.2 unit (maximum sectoral PiN)
overall_pin <- data %>%
  group_by(Admin.2.Pcode) %>%
  summarise(overall_pin = max(pin, na.rm = TRUE))

# Step 2: Calculate average and median PiN for each severity level across all sectors
avg_median_pin_severity <- data %>%
  group_by(Admin.2.Pcode) %>%
  summarise(
    across(ends_with("5"), ~ mean(as.numeric(.), na.rm = TRUE), .names = "{.col}_avg"),
    across(ends_with("4"), ~ mean(as.numeric(.), na.rm = TRUE), .names = "{.col}_avg"),
    across(ends_with("3"), ~ mean(as.numeric(.), na.rm = TRUE), .names = "{.col}_avg"),
    #across(ends_with("2"), ~ mean(as.numeric(.), na.rm = TRUE), .names = "{.col}_avg"),
    #across(ends_with("1"), ~ mean(as.numeric(.), na.rm = TRUE), .names = "{.col}_avg"),
    across(ends_with("5"), ~ median(as.numeric(.), na.rm = TRUE), .names = "{.col}_median"),
    across(ends_with("4"), ~ median(as.numeric(.), na.rm = TRUE), .names = "{.col}_median"),
    across(ends_with("3"), ~ median(as.numeric(.), na.rm = TRUE), .names = "{.col}_median"),
    #across(ends_with("2"), ~ median(as.numeric(.), na.rm = TRUE), .names = "{.col}_median"),
    #across(ends_with("1"), ~ median(as.numeric(.), na.rm = TRUE), .names = "{.col}_median")
  ) %>%
  ungroup()

# Combine average and median results for final output
final_severity_distribution <- avg_median_pin_severity %>%
  inner_join(overall_pin, by = c("Admin.2.Pcode"))

# Print the result
print(final_severity_distribution)

# Save the updated data with percentages to a new CSV file
write.csv(final_severity_distribution, "process/6.AverageAndMedianSeverityDistributionMosaic.csv", row.names = FALSE)