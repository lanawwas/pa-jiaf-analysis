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

# First Step: Select the sector with the highest PiN for Admin.2(Lowest level of Analysis)
highest_pin_sectors <- data %>%
  group_by(Admin.2.Pcode) %>%
  filter(pin == max(pin)) %>%
  distinct(Admin.2.Pcode, .keep_all = TRUE)  # Keep all columns for the distinct rows

# Check the structure of the data
str(highest_pin_sectors)

# Save the updated data with percentages to a new CSV file
write.csv(highest_pin_sectors, "process/4.modified_data_highest_sector_pin.csv", row.names = FALSE)