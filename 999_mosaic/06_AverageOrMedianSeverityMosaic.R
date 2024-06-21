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
    across(ends_with("2"), ~ mean(as.numeric(.), na.rm = TRUE), .names = "{.col}_avg"),
    across(ends_with("1"), ~ mean(as.numeric(.), na.rm = TRUE), .names = "{.col}_avg"),
    across(ends_with("5"), ~ median(as.numeric(.), na.rm = TRUE), .names = "{.col}_median"),
    across(ends_with("4"), ~ median(as.numeric(.), na.rm = TRUE), .names = "{.col}_median"),
    across(ends_with("3"), ~ median(as.numeric(.), na.rm = TRUE), .names = "{.col}_median"),
    across(ends_with("2"), ~ median(as.numeric(.), na.rm = TRUE), .names = "{.col}_median"),
    across(ends_with("1"), ~ median(as.numeric(.), na.rm = TRUE), .names = "{.col}_median"),
    overall_pin = max(pin, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2.2: Calculate the sum of severity levels 5 and 4
avg_median_pin_severity_adjusted <- avg_median_pin_severity %>%
  rowwise() %>%
  mutate(
    severity5_avg_sum = sum(c_across(ends_with("5_avg")), na.rm = TRUE),
    severity4_avg_sum = sum(c_across(ends_with("4_avg")), na.rm = TRUE),
    severity5_median_sum = sum(c_across(ends_with("5_median")), na.rm = TRUE),
    severity4_median_sum = sum(c_across(ends_with("4_median")), na.rm = TRUE)
  ) %>%
  ungroup()

# Step 3: Calculate the PiN for severity level 3 using the adjusted formula
avg_median_pin_severity <- avg_median_pin_severity_adjusted %>%
  mutate(
    across(ends_with("3_avg"), ~ (overall_pin - (severity5_avg_sum + severity4_avg_sum)), .names = "{.col}_residual"),
    across(ends_with("3_median"), ~ (overall_pin - (severity5_median_sum + severity4_median_sum)), .names = "{.col}_residual")
  ) %>%
  dplyr::select(-severity5_avg_sum, -severity4_avg_sum, -severity5_median_sum, -severity4_median_sum) # Drop the sum columns as they are no longer needed

# Print the resulting dataframe
print(avg_median_pin_severity)

# Step 3: Express the values stored in each severity level column as a percentage of the overall PiN
severity_distribution_final <- avg_median_pin_severity %>%
  mutate(
    across(contains("_"), ~ .x / overall_pin * 100, .names = "{.col}_percentage")
  )%>%
  dplyr::select(-overall_pin_percentage)

# Print the result
print(severity_distribution_final)

# Combine average and median results for final output
#final_severity_distribution <- avg_median_pin_severity %>%
#  inner_join(overall_pin, by = c("Admin.2.Pcode"))

# Print the result
#print(final_severity_distribution)

# Save the updated data with percentages to a new CSV file
write.csv(severity_distribution_final, "process/6.AverageAndMedianSeverityDistributionMosaic.csv", row.names = FALSE)