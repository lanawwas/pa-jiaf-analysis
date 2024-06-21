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
    across(ends_with("2"), ~ max(as.numeric(.), na.rm = TRUE), .names = "{.col}_max"),
    across(ends_with("1"), ~ max(as.numeric(.), na.rm = TRUE), .names = "{.col}_max")
  ) %>%
  ungroup()

# Step 2B: Adjust the calculation for severity level "3_residual"
max_pin_severity <- max_pin_severity %>%
  inner_join(overall_pin, by = "Admin.2.Pcode") %>%
  rowwise() %>%
  mutate(
    across(ends_with("3_max"), ~ overall_pin - sum(c_across(ends_with("5_max")), c_across(ends_with("4_max")), na.rm = TRUE), .names = "{.col}_residual")
  ) %>%
  ungroup()

# Step 3: Allocate the maximum PiN for each severity level until the overall PiN reaches zero 
severity_distribution_allocation <- max_pin_severity %>%
  rowwise() %>%
  mutate(
    residual = overall_pin,
    stop_allocation = FALSE,
    
    # Allocate for severity 5
    across(ends_with("5_max"), ~ ifelse(residual >= .x & !stop_allocation, .x, NA_real_), .names = "{.col}"),
    residual = residual - sum(c_across(ends_with("5_max")), na.rm = TRUE),
    stop_allocation = stop_allocation | any(is.na(c_across(ends_with("5_max_allocated")))),
    
    # Allocate for severity 4
    across(ends_with("4_max"), ~ ifelse(residual >= .x & !stop_allocation, .x, NA_real_), .names = "{.col}"),
    residual = residual - sum(c_across(ends_with("4_max")), na.rm = TRUE),
    stop_allocation = stop_allocation | any(is.na(c_across(ends_with("4_max")))),
    
    # Allocate for severity 3
    across(ends_with("3_max"), ~ ifelse(residual >= .x & !stop_allocation, .x, NA_real_), .names = "{.col}"),
    residual = residual - sum(c_across(ends_with("3_max")), na.rm = TRUE),
    stop_allocation = stop_allocation | any(is.na(c_across(ends_with("3_max")))),
    
    # Allocate for severity 2
    across(ends_with("2_max"), ~ ifelse(residual >= .x & !stop_allocation, .x, NA_real_), .names = "{.col}"),
    residual = residual - sum(c_across(ends_with("2_max")), na.rm = TRUE),
    stop_allocation = stop_allocation | any(is.na(c_across(ends_with("2_max")))),
    
    # Allocate for severity 1
    across(ends_with("1_max"), ~ ifelse(residual >= .x & !stop_allocation, .x, NA_real_), .names = "{.col}")
  ) %>%
  ungroup()

# Display the allocation
print(severity_distribution_allocation)


# Step 3: Express the values stored in each severity level column as a percentage of the overall PiN
severity_distribution_final <- severity_distribution_allocation %>%
  mutate(
    across(contains("_"), ~ .x / overall_pin * 100, .names = "{.col}_percentage")
  )%>%
  dplyr::select(-stop_allocation_percentage, -overall_pin_percentage)

# Print the result
print(severity_distribution_final)

# Save the updated data with percentages to a new CSV file
write.csv(severity_distribution_final, "process/5.OverallSeverityDistributionMosaic.csv", row.names = FALSE)