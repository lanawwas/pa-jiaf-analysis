# Install required packages if not already installed
if (!require("tidyverse")) {
  install.packages("tidyverse")
}
if (!require("car")) {
  install.packages("car")
}
if (!require("multcomp")) {
  install.packages("multcomp")
}

if (!require("irr")) {
  install.packages("irr")
}

if (!require("stats")) {
  install.packages("stats")
}

# Load necessary libraries
library(tidyverse)
library(car)  # For ANOVA
library(multcomp)  # For post-hoc tests
library(irr) # For ICC
library(stats)  # For Kolmogorov-Smirnov test


# Read the CSV file
method4 <- read.csv("process/4.modified_data_highest_sector_pin.csv")
method5 <- read.csv("process/5.OverallSeverityDistributionMosaic.csv")
method6 <- read.csv("process/6.AverageAndMedianSeverityDistributionMosaic.csv")

# View the data structure
str(method4)
str(method5)
str(method6)

# Standardize method5 and method6 to match method4's structure
method5_long <- method5 %>%
  dplyr::select(Admin.2.Pcode, dummy_pin_5_max, dummy_pin_4_max, dummy_pin_3_max) %>%
  pivot_longer(cols = starts_with("dummy_pin_"), names_to = "Severity", values_to = "Count") %>%
  mutate(Method = "method5", Severity = as.integer(str_extract(Severity, "\\d")))

method6_long <- method6 %>%
  dplyr::select(Admin.2.Pcode, dummy_pin_5_avg, dummy_pin_4_avg, dummy_pin_3_avg) %>%
  pivot_longer(cols = starts_with("dummy_pin_"), names_to = "Severity", values_to = "Count") %>%
  mutate(Method = "method6", Severity = as.integer(str_extract(Severity, "\\d")))

method4_long <- method4 %>%
  dplyr::select(Admin.2.Pcode, dummy_pin_1, dummy_pin_2, dummy_pin_3, dummy_pin_4, dummy_pin_5) %>%
  pivot_longer(cols = starts_with("dummy_pin_"), names_to = "Severity", values_to = "Count") %>%
  mutate(Method = "method4", Severity = as.integer(str_extract(Severity, "\\d")))

# Combine all methods into a single dataframe
combined_data <- bind_rows(method4_long, method5_long, method6_long)

# View combined data
head(combined_data)

# Test 1: Summary statistics by method and severity
summary_stats <- combined_data %>%
  group_by(Method, Severity) %>%
  summarise(Mean = mean(Count, na.rm = TRUE), SD = sd(Count, na.rm = TRUE), Median = median(Count, na.rm = TRUE), IQR = IQR(Count, na.rm = TRUE))

print(summary_stats)

# Test 2:  Perform ANOVA
anova_result <- aov(Count ~ Method * Severity, data = combined_data)
summary(anova_result)

# Post-hoc tests if ANOVA is significant
#posthoc <- glht(anova_result, linfct = mcp(Method = "Tukey"))
#summary(posthoc)

# Test 3: Visulization

# Boxplot
ggplot(combined_data, aes(x = as.factor(Severity), y = Count, fill = Method)) +
  geom_boxplot() +
  labs(title = "Distribution of Counts by Method and Severity Level", x = "Severity Level", y = "Count")

# Density plot
ggplot(combined_data, aes(x = Count, fill = Method)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Severity) +
  labs(title = "Density Plot of Counts by Method and Severity Level", x = "Count", y = "Density")

# Test 4: Goodness-of-fit test (Chi-squared test)

# Calculate expected frequencies based on overall mean
expected_counts <- combined_data %>%
  group_by(Severity) %>%
  summarise(Expected = mean(Count, na.rm = TRUE))

# Join with the combined data
combined_data <- combined_data %>%
  left_join(expected_counts, by = "Severity")

# Calculate chi-squared goodness-of-fit for each method and severity
gof_results <- combined_data %>%
  group_by(Method, Severity) %>%
  summarise(Observed = sum(Count, na.rm = TRUE),
            Expected = sum(Expected, na.rm = TRUE)) %>%
  mutate(ChiSq = (Observed - Expected)^2 / Expected) %>%
  summarise(ChiSq = sum(ChiSq))

print(gof_results)

# Test 5: Perform Kolmogorov-Smirnov test between methods
ks_results <- list()

methods <- unique(combined_data$Method)

for (i in 1:(length(methods) - 1)) {
  for (j in (i + 1):length(methods)) {
    method1 <- combined_data %>% filter(Method == methods[i]) %>% pull(Count)
    method2 <- combined_data %>% filter(Method == methods[j]) %>% pull(Count)
    ks_test <- ks.test(method1, method2)
    ks_results[[paste(methods[i], methods[j], sep = "_vs_")]] <- ks_test$p.value
  }
}

# Print Kolmogorov-Smirnov test results
ks_results

# Test 6: Convert combined data to wide format for ICC calculation
combined_wide <- combined_data %>%
  pivot_wider(names_from = Method, values_from = Count) %>%
  dplyr::select(-c(Admin.2.Pcode, Severity))

# Intraclass correlation coefficient (ICC)
icc_result <- icc(combined_wide, model = "twoway", type = "consistency", unit = "average")
print(icc_result)