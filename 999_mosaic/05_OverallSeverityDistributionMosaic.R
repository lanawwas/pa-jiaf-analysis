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

# Print the structure of the data
str(data)

# Step 1: Calculate overall PiN for each admin.2 unit (maximum sectoral PiN)
overall_pin <- data %>%
  group_by(Admin.2.Pcode) %>%
  summarise(overall_pin = max(pin, na.rm = TRUE))

# Step 2: Calculate maximum PiN for each severity level across all sectors
max_pin_severity <- data %>%
  group_by(Admin.2.Pcode) %>%
  summarise(
    across(ends_with("5"), ~ max(as.numeric(.), na.rm = TRUE), .names = "{.col}_max"),
    across(ends_with("4"), ~ max(as.numeric(.), na.rm = TRUE), .names = "{.col}_max"),
    across(ends_with("3"), ~ max(as.numeric(.), na.rm = TRUE), .names = "{.col}_max"),
    #across(ends_with("2"), ~ max(as.numeric(.), na.rm = TRUE), .names = "{.col}_max"),
    #across(ends_with("1"), ~ max(as.numeric(.), na.rm = TRUE), .names = "{.col}_max")
  ) %>%
  ungroup()

# Step 2B: Adjust the calculation for severity level "3"
max_pin_severity <- max_pin_severity %>%
  inner_join(overall_pin, by = "Admin.2.Pcode") %>%
  rowwise() %>%
  mutate(
    across(ends_with("3_max"), ~ overall_pin - sum(c_across(ends_with("5_max")), c_across(ends_with("4_max")), na.rm = TRUE), .names = "{.col}")
  ) %>%
  ungroup()

# Step 3: Express the values stored in each severity level column as a percentage of the overall PiN
severity_distribution <- max_pin_severity %>%
  mutate(
    across(ends_with("_max"), ~ .x / overall_pin * 100, .names = "{.col}_percentage")
  )

# Print the result
print(severity_distribution)

# Save the updated data with percentages to a new CSV file
write.csv(severity_distribution, "process/5.OverallSeverityDistributionMosaic.csv", row.names = FALSE)