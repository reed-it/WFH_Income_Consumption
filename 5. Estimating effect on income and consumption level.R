# Difference-in-Differences Analysis: WFH Impact on Income and Consumption
# Author: Analysis Script
# Date: July 12, 2025

# Load required libraries
library(tidyverse)
library(fixest)
library(modelsummary)
library(gt)
library(broom)
library(sandwich)

# Load the data
data <- read_csv("LCFS_DiD_data_filtered3_from_2018.csv")

# Clean start - remove any existing variables that might cause conflicts
if("treated" %in% names(data)) data$treated <- NULL
if("treat_group" %in% names(data)) data$treat_group <- NULL

# Quick data overview
cat("Dataset Overview:\n")
cat("Observations:", nrow(data), "\n")
cat("Variables:", ncol(data), "\n")
cat("\nSummary Statistics:\n")
summary(data)

# Check treatment and control group sizes
cat("\nTreatment Group Distribution:\n")
table(data$treatment_1, useNA = "always")

cat("\nWFH Correlation Distribution:\n")
table(data$WfhCor_1, useNA = "always")

cat("\nDiD Effect Distribution:\n")
table(data$did_effect, useNA = "always")

# =============================================================================
# DIFFERENCE-IN-DIFFERENCES ANALYSIS
# =============================================================================

# Model 1: Basic DiD for Income
# Using the pre-calculated did_effect variable
model_income_basic <- feols(income ~ did_effect, data = data, vcov = "hetero")

# Model 2: DiD for Income with year fixed effects
model_income_fe <- feols(income ~ did_effect + year_2018 + year_2019 + 
                           year_2020 + year_2021 + year_2022, 
                         data = data, vcov = "hetero")

# Model 3: Manual DiD construction for Income (alternative specification)
model_income_manual <- feols(income ~ treatment_1 * WfhCor_1, data = data, vcov = "hetero")

# Model 4: Manual DiD with year fixed effects
model_income_manual_fe <- feols(income ~ treatment_1 * WfhCor_1 + year_2018 +
                                  year_2019 + year_2020 + year_2021 + year_2022, 
                                data = data, vcov = "hetero")

# =============================================================================
# CONSUMPTION ANALYSIS
# =============================================================================

# Model 5: Basic DiD for Consumption
model_consumption_basic <- feols(derived_consumption ~ did_effect, data = data, vcov = "hetero")

# Model 6: DiD for Consumption with year fixed effects
model_consumption_fe <- feols(derived_consumption ~ did_effect + year_2018 +
                                year_2019 + year_2020 + year_2021 + year_2022, 
                              data = data, vcov = "hetero")

# Model 7: Manual DiD construction for Consumption
model_consumption_manual <- feols(derived_consumption ~ treatment_1 * WfhCor_1, data = data, vcov = "hetero")

# Model 8: Manual DiD with year fixed effects
model_consumption_manual_fe <- feols(derived_consumption ~ treatment_1 * WfhCor_1  + year_2018 + year_2019 + year_2020 + year_2021 + year_2022, 
                                     data = data, vcov = "hetero")

# =============================================================================
# RESULTS PRESENTATION
# =============================================================================

# Create summary tables
cat("\n=== INCOME ANALYSIS RESULTS ===\n")
income_models <- list(
  "Basic DiD" = model_income_basic,
  "DiD + Year FE" = model_income_fe,
  "Manual DiD" = model_income_manual,
  "Manual DiD + Year FE" = model_income_manual_fe
)

# First, let's check the coefficient names in our models
cat("Coefficient names in models:\n")
cat("Model 1 (Basic DiD):", names(coef(model_income_basic)), "\n")
cat("Model 2 (DiD + Year FE):", names(coef(model_income_fe)), "\n")
cat("Model 3 (Manual DiD):", names(coef(model_income_manual)), "\n")
cat("Model 4 (Manual DiD + Year FE):", names(coef(model_income_manual_fe)), "\n\n")

# Print income results without coef_map first to see all coefficients
modelsummary(income_models, 
             title = "Difference-in-Differences: Impact of WFH on Income",
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))

cat("\n=== CONSUMPTION ANALYSIS RESULTS ===\n")
consumption_models <- list(
  "Basic DiD" = model_consumption_basic,
  "DiD + Year FE" = model_consumption_fe,
  "Manual DiD" = model_consumption_manual,
  "Manual DiD + Year FE" = model_consumption_manual_fe
)

# Print consumption results without coef_map first to see all coefficients
modelsummary(consumption_models, 
             title = "Difference-in-Differences: Impact of WFH on Consumption",
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))

# =============================================================================
# PARALLEL TRENDS ASSUMPTION TESTS
# =============================================================================

cat("\n=== PARALLEL TRENDS ASSUMPTION TESTING ===\n")

# First, let's understand the treatment timing
# Assuming WfhCor_1 indicates the post-treatment period (when WFH became prevalent)
# and treatment_1 indicates the treatment group

# Create a long dataset for trend analysis
data_long <- data %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "indicator") %>%
  filter(indicator == TRUE) %>%
  mutate(
    year_num = as.numeric(str_extract(year, "\\d{4}")),
    # WfhCor_1 indicates treatment group (who can WFH)
    # treatment_1 indicates post-treatment period (when WFH policies implemented)
    treatment_group = ifelse(WfhCor_1 == TRUE, 1, 0),
    post_treatment = ifelse(treatment_1 == TRUE, 1, 0)
  )

# Create treatment group indicator
data_long$treat_group <- paste0("WFH_Group_", data_long$treatment_group, "_Post_", data_long$post_treatment)

# =============================================================================
# TEST 1: VISUAL INSPECTION OF PARALLEL TRENDS
# =============================================================================

# Calculate mean outcomes by year and treatment group (WfhCor_1)
yearly_means <- data_long %>%
  group_by(year_num, treatment_group) %>%
  summarise(
    mean_income = mean(income, na.rm = TRUE),
    mean_consumption = mean(derived_consumption, na.rm = TRUE),
    n = n(),
    se_income = sd(income, na.rm = TRUE) / sqrt(n()),
    se_consumption = sd(derived_consumption, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# Determine treatment year from the data
treatment_years <- data_long %>% 
  filter(post_treatment == 1) %>% 
  pull(year_num) %>% 
  unique() %>% 
  min()

cat("Treatment period begins in year:", treatment_years, "\n\n")

# Plot parallel trends for income
library(ggplot2)

p_income_trends <- ggplot(yearly_means, aes(x = year_num, y = mean_income, 
                                            color = factor(treatment_group), group = factor(treatment_group))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_income - 1.96*se_income, 
                    ymax = mean_income + 1.96*se_income), width = 0.2) +
  # Add vertical line at treatment year
  geom_vline(xintercept = treatment_years, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = treatment_years, y = max(yearly_means$mean_income) * 0.95, 
           label = "WFH Policy\nImplemented", hjust = -0.1, color = "red") +
  labs(title = "Parallel Trends Test: Income Over Time",
       subtitle = "Pre-treatment trends should be parallel between WFH-eligible and non-eligible groups",
       x = "Year",
       y = "Mean Income",
       color = "Treatment Group") +
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("Non-WFH Eligible", "WFH Eligible")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot parallel trends for consumption
p_consumption_trends <- ggplot(yearly_means, aes(x = year_num, y = mean_consumption, 
                                                 color = factor(treatment_group), group = factor(treatment_group))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_consumption - 1.96*se_consumption, 
                    ymax = mean_consumption + 1.96*se_consumption), width = 0.2) +
  geom_vline(xintercept = treatment_years, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = treatment_years, y = max(yearly_means$mean_consumption) * 0.95, 
           label = "WFH Policy\nImplemented", hjust = -0.1, color = "red") +
  labs(title = "Parallel Trends Test: Consumption Over Time",
       subtitle = "Pre-treatment trends should be parallel between WFH-eligible and non-eligible groups",
       x = "Year",
       y = "Mean Consumption",
       color = "Treatment Group") +
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("Non-WFH Eligible", "WFH Eligible")) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_income_trends)
print(p_consumption_trends)

# =============================================================================
# TEST 4: ALTERNATIVE APPROACH - INTERACTION WITH TIME TREND
# =============================================================================

# Test if treatment and control groups have different time trends
cat("\n=== TIME TREND INTERACTION TEST ===\n")

# Create time trend variable
data_long$time_trend <- data_long$year_num - min(data_long$year_num)

# Test for differential trends in pre-treatment period
pre_treatment_data <- data_long %>% filter(year_num < treatment_years)

# Income trend test - testing if WFH-eligible and non-eligible groups have different time trends
trend_test_income <- feols(income ~ treatment_group * time_trend + factor(year_num), 
                           data = pre_treatment_data, vcov = "hetero")

# Consumption trend test  
trend_test_consumption <- feols(derived_consumption ~ treatment_group * time_trend + factor(year_num), 
                                data = pre_treatment_data, vcov = "hetero")

cat("Pre-treatment trend test for INCOME:\n")
print(summary(trend_test_income))

cat("Pre-treatment trend test for CONSUMPTION:\n")
print(summary(trend_test_consumption))

# =============================================================================
# TEST 4: COEFFICIENT PLOT FOR VISUAL INSPECTION
# =============================================================================

# Since we removed the event study, let's create a simple trend comparison plot
cat("\n=== TREND COMPARISON VISUALIZATION ===\n")

# Create trend comparison plot showing pre vs post treatment periods
trend_comparison <- data_long %>%
  mutate(period = ifelse(year_num < treatment_years, "Pre-Treatment", "Post-Treatment")) %>%
  group_by(period, treatment_group) %>%
  summarise(
    mean_income = mean(income, na.rm = TRUE),
    mean_consumption = mean(derived_consumption, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )

print(trend_comparison)

# =============================================================================
# INTERPRETATION GUIDE
# =============================================================================

cat("\n=== PARALLEL TRENDS ASSUMPTION INTERPRETATION ===\n")
cat("CORRECT INTERPRETATION FOR YOUR STUDY:\n")
cat("- WfhCor_1 = Treatment Group (WFH-eligible vs. non-eligible workers)\n")
cat("- treatment_1 = Post-Treatment Period (when WFH policies were implemented)\n\n")

cat("1. VISUAL TEST: Look at the trend plots above.\n")
cat("   - Pre-treatment trends should be parallel between WFH-eligible and non-eligible groups\n")
cat("   - If lines diverge before WFH policy implementation, parallel trends may be violated\n\n")

cat("4. TREND INTERACTION: Check if WFH_group*time_trend is significant in pre-period.\n")
cat("   - Non-significant interaction suggests parallel trends hold\n")
cat("   - Significant interaction suggests WFH-eligible and non-eligible groups had different trends\n\n")

cat("WHAT YOU'RE TESTING:\n")
cat("- Whether WFH-eligible and non-eligible workers had similar outcome trends\n")
cat("- BEFORE the implementation of WFH policies\n")
cat("- This validates using the policy implementation as a quasi-experimental shock\n\n")

# =============================================================================
# ROBUSTNESS CHECKS
# =============================================================================

# Robustness: Clustered standard errors (if you have cluster variable)
# Uncomment and modify if you have clustering variable
# model_income_cluster <- feols(income ~ did_effect + year_2017 + year_2018 + 
#                              year_2019 + year_2020 + year_2021 + year_2022, 
#                              data = data, vcov = ~cluster_var)

# Additional robustness: Log transformations (if appropriate)
# Check if income and consumption have positive values
if(all(data$income > 0, na.rm = TRUE) & all(data$derived_consumption > 0, na.rm = TRUE)) {
  cat("\n=== LOG TRANSFORMATION MODELS ===\n")
  
  model_log_income <- feols(log(income) ~ did_effect + year_2017 + year_2018 + 
                              year_2019 + year_2020 + year_2021 + year_2022, 
                            data = data, vcov = "hetero")
  
  model_log_consumption <- feols(log(derived_consumption) ~ did_effect + year_2017 + 
                                   year_2018 + year_2019 + year_2020 + year_2021 + year_2022, 
                                 data = data, vcov = "hetero")
  
  log_models <- list(
    "Log Income" = model_log_income,
    "Log Consumption" = model_log_consumption
  )
  
  modelsummary(log_models, 
               title = "Log-Transformed Models",
               stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
               gof_map = c("nobs", "r.squared", "adj.r.squared"))
}

# =============================================================================
# VISUALIZATION
# =============================================================================

# Create visualization of treatment effects
library(ggplot2)

# Plot 1: Income by treatment status
p1 <- ggplot(data, aes(x = factor(treatment_1), y = income, fill = factor(WfhCor_1))) +
  geom_boxplot() +
  labs(title = "Income Distribution by Treatment and WFH Status",
       x = "Treatment Status",
       y = "Income",
       fill = "WFH") +
  theme_minimal()

# Plot 2: Consumption by treatment status
p2 <- ggplot(data, aes(x = factor(treatment_1), y = derived_consumption, fill = factor(WfhCor_1))) +
  geom_boxplot() +
  labs(title = "Consumption Distribution by Treatment and WFH Status",
       x = "Treatment Status",
       y = "Derived Consumption",
       fill = "WFH") +
  theme_minimal()

# Display plots
print(p1)
print(p2)

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Key DiD Coefficients:\n")
cat("Income Effect:", round(coef(model_income_fe)["did_effect"], 3), "\n")
cat("Income p-value:", round(summary(model_income_fe)$coeftable["did_effect", "Pr(>|t|)"], 4), "\n")
cat("Consumption Effect:", round(coef(model_consumption_fe)["did_effect"], 3), "\n")
cat("Consumption p-value:", round(summary(model_consumption_fe)$coeftable["did_effect", "Pr(>|t|)"], 4), "\n")

cat("\nNote: This analysis assumes the parallel trends assumption holds.")
cat("\nConsider additional robustness checks and pre-trend analysis if needed.")
