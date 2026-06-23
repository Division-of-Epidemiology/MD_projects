##looking at changes across zip codes over time (2016-2024 eventually)
library(MASS)
library(dplyr)
library(tidyr)
library(stringr)
library(AER)
library(car)
library(ggplot2)
library(ggeffects)
library(gridExtra)
library(boot)
library(knitr)
library(kableExtra)
library(formattable)
library(broom)
library(purrr)


####Tracking risk level over time broken down by census tract
getwd()
ct_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/CDC dashbaord data/Modeling_scripts/CT_data_model.csv")

# Calculate risk classification for each CT
ct_risk_classification <- ct_data %>%
  group_by(Unique_ID) %>%
  summarise(
    income = mean(median_Household_Income, na.rm = TRUE),
    uninsured = mean(uninsured_percentage, na.rm = TRUE),
    low_ed = mean(high_school, na.rm = TRUE),
    home_values = mean(median_home_value, na.rm = TRUE),
    food_insecurity = mean(crd_foodinsecu, na.rm = TRUE),
    poverty = mean(percent_poverty, na.rm = TRUE),
    unemployment = sum(unemploy_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate thresholds within each CT
  mutate(
    # Count risk factors using specific thresholds
    risk_score = 
      (income < quantile(income, 0.33, na.rm = TRUE)) +
      (uninsured > quantile(uninsured, 0.67, na.rm = TRUE)) +
      (low_ed > quantile(low_ed, 0.67, na.rm = TRUE)) +
      (home_values < quantile(home_values, 0.33, na.rm = TRUE)) +
      (food_insecurity > quantile(food_insecurity, 0.67, na.rm = TRUE)) +
      (poverty > quantile(poverty, 0.67, na.rm = TRUE)) +
      (unemployment > quantile(unemployment, 0.67, na.rm = TRUE)),
    
    # Create risk tiers
    risk_tier = case_when(
      risk_score >= 5 ~ "Very High Risk",
      risk_score >= 3 ~ "High Risk",
      risk_score >= 2 ~ "Moderate Risk",
      TRUE ~ "Low Risk"
    ),
    risk_tier = factor(risk_tier, levels = c("Low Risk", "Moderate Risk", 
                                             "High Risk", "Very High Risk")),
    
    # Binary flag
    high_risk_zip = risk_score >= 3
  ) %>%
  ungroup()

# Check how many zips are in each tier by year
ct_risk_classification %>%
  count(risk_tier) %>%
  pivot_wider(names_from = risk_tier, values_from = n)


write.csv(ct_risk_classification, "C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/CT_risk_data.csv")

