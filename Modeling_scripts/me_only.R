##ME ONLY MODELING
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
library(purrr)

##Doing step wise on the model that fits best to make sure variables are a good fit
me_complete <- glm.nb(
  Total_ME ~ median_age + median_income + low.ed + uninsured + 
    Total_narcan + rpopden + Non_violent + post_2023 + high_risk_zip +
    Total_narcan:uninsured, 
  data=zip_change,
  link=log
)


step_model_me <- stepAIC(me_complete, direction = "both")




###Model 1: standard negative binomial with year effects
model_d <- glm.nb(
  Total_ME ~ median_income + low.ed + Total_narcan + rpopden + 
    Non_violent + post_2023 +
    factor(Year),
  data=zip_change,
  link=log
)


model_e <- glm.nb(
  Total_ME ~ median_income + low.ed + Total_narcan + rpopden + 
    Non_violent + post_2023 + time_trend,
  data=zip_change,
  link=log
)


model_f <- glm.nb(
  Total_ME ~ median_income + low.ed + Total_narcan + rpopden + 
    Non_violent + post_2023 +
    high_risk_zip:post_2023 +
    Total_narcan:uninsured, 
  data=zip_change,
  link=log
)

summary(model_c)

models <- list(
  "Year Effects" = model_d,
  "Trend + Break" = model_e,
  "Geographic Interactions" = model_f
)

model_comparison <- data.frame(
  Model = names(models),
  AIC = sapply(models, AIC),
  Deviance = sapply(models, function(m) m$deviance),
  stringsAsFactors = FALSE
) %>%
  arrange(AIC)

print("MODEL COMPARISON (lower AIC = better):")
print(model_comparison)

# Print best model summary
best_model <- models[[model_comparison$Model[1]]]
print("BEST MODEL SUMMARY:")
print(summary(best_model)$coefficients)


##model E is the best one so we will continue with that one for the predictions!

# Time-based split (most realistic for your use case)
train_data <- zip_change[zip_change$Year %in% c(2021, 2022, 2023), ]
test_data <- zip_change[zip_change$Year == 2024, ]

# Refit model on training data only
model_validation_me <- glm.nb(
  Total_ME ~ median_income + low.ed + Total_narcan + rpopden + 
    Non_violent + post_2023 + time_trend,
  data=train_data,
  link=log)

# Test predictions on 2024 data
test_predictions <- predict(model_validation_me, newdata = test_data, se.fit = TRUE)

# Compare predicted vs actual
validation_results_me <- test_data %>%
  mutate(
    predicted = exp(test_predictions$fit),
    actual = Total_ME,
    error = actual - predicted,
    abs_error = abs(error),
    pct_error = abs(error) / actual * 100
  )

# Summary statistics
mean(validation_results_me$abs_error, na.rm = TRUE)  # Mean absolute error
mean(validation_results_me$pct_error, na.rm = TRUE)  # Mean percentage error
cor(validation_results_me$predicted, validation_results_me$actual)  # Correlation


# Check the scale of your EMS calls to contextualize the error
summary(validation_results_me$actual)  # What's the typical range?
median(validation_results_me$actual)   # Median actual calls


create_scenario_predictions <- function(model, years = 2025:2030) {
  
  # Base year data - preserve variable types
  base_data <- zip_change %>%
    filter(Year == max(Year)) %>%
    group_by(Zip) %>%
    summarise(
      median_income = mean(median_income, na.rm = TRUE),
      low.ed = mean(low.ed, na.rm = TRUE),
      Total_narcan = mean(Total_narcan, na.rm = TRUE),
      rpopden = mean(rpopden, na.rm = TRUE),
      Non_violent = mean(Non_violent, na.rm = TRUE),
      post_2023 = mean(as.numeric(post_2023), na.rm = TRUE),
      .groups = "drop"
    )
  
  scenarios <- list(
    optimistic = list(
      Total_narcan = 0.90,      # 10% decrease in narcan
      Non_violent = 0.90          # 5% decrease in uninsured
    ),
    pessimistic = list(
      Total_narcan = 1.10,      # 10% increase in narcan 
      Non_violent = 1.10          # 10% increase in uninsured
    ),
    baseline = list(
      Total_narcan = 1.00,      # No change
      Non_violent = 1.00
    )
  )
  
  all_predictions <- map_dfr(names(scenarios), function(scenario_name) {
    scenario_data <- base_data %>%
      mutate(
        Total_narcan = Total_narcan * scenarios[[scenario_name]]$Total_narcan,
        Non_violent = Non_violent * scenarios[[scenario_name]]$Non_violent
      )
    
    map_dfr(years, function(year) {
      # Add time_trend for the prediction year
      scenario_data_year <- scenario_data %>%
        mutate(
          Year = year,
          time_trend = year - 2021  # Consistent with how you defined it
        )
      
      pred <- predict(model, newdata = scenario_data_year, se.fit = TRUE)
      
      scenario_data_year %>%
        mutate(
          scenario = scenario_name,
          predicted_me = exp(pred$fit),
          lower_ci = exp(pred$fit - 1.96 * pred$se.fit),
          upper_ci = exp(pred$fit + 1.96 * pred$se.fit)
        )
    })
  })
  
  return(all_predictions)
}

scenario_predictions_me <- create_scenario_predictions(model_e)
####trying to combine above with time component
scenarios <- list(
  optimistic = list(
    median_income = 1.10,
    Total_narcan = 1.10,
    low.ed = 0.95,
    rpopden = 1.10,
    Non_violent = 0.90
  ),
  pessimistic = list(
    median_income = 0.90,
    Total_narcan = 0.90,
    low.ed = 1.05,
    rpopden = 0.90,
    Non_violent = 1.10
  ),
  baseline = list(
    median_income = 1.00,
    Total_narcan = 1.00,
    low.ed = 1.00,
    rpopden = 1.00,
    Non_violent = 1.00
  )
)

create_trending_predictions <- function(model, years = 2025:2030, scenarios = NULL) {
  
  if (is.null(scenarios)) {
    scenarios <- list(baseline = list(
      median_income = 1.00,
      Total_narcan = 1.00,
      low.ed = 1.00,
      rpopden = 1.00,
      Non_violent = 1.00
    ))
  }
  
  # Fit OVERALL trends across all data (not by zip)
  overall_trends <- list(
    income_slope = coef(lm(median_income ~ Year, data = zip_change))[2],
    income_intercept = coef(lm(median_income ~ Year, data = zip_change))[1],
    narcan_slope = coef(lm(Total_narcan ~ Year, data = zip_change))[2],
    narcan_intercept = coef(lm(Total_narcan ~ Year, data = zip_change))[1],
    pop_slope = coef(lm(rpopden ~ Year, data = zip_change))[2],
    pop_intercept = coef(lm(rpopden ~ Year, data = zip_change))[1],
    crime_slope = coef(lm(Non_violent ~ Year, data = zip_change))[2],
    crime_intercept = coef(lm(Non_violent ~ Year, data = zip_change))[1]
  )
  
  # Get baseline values for each zip from most recent year
  zip_baseline <- zip_change %>%
    filter(Year == max(Year)) %>%
    group_by(Zip) %>%
    summarise(
      median_income_base = mean(median_income, na.rm = TRUE),
      narcan_base = mean(Total_narcan, na.rm = TRUE),
      pop_base = mean(rpopden, na.rm = TRUE),
      crime_base = mean(Non_violent, na.rm = TRUE),
      low_ed_avg = mean(low.ed, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Loop over scenarios then years
  all_predictions <- map_dfr(names(scenarios), function(scenario_name) {
    scenario <- scenarios[[scenario_name]]
    
    map_dfr(years, function(year) {
      # Apply overall trends from baseline year (2024)
      year_change <- year - 2024
      
      projected_data <- zip_baseline %>%
        mutate(
          Year = year,
          time_trend = year - 2021,
          post_2023 = ifelse(year >= 2024, 1, 0),
          median_income = pmax(0, median_income_base + overall_trends$income_slope * year_change) * scenario$median_income,
          Total_narcan = pmax(0, narcan_base + overall_trends$narcan_slope * year_change) * scenario$Total_narcan,
          rpopden = pmax(0, pop_base + overall_trends$pop_slope * year_change) * scenario$rpopden,
          Non_violent = pmax(0, crime_base + overall_trends$crime_slope * year_change) * scenario$Non_violent,
          low.ed = low_ed_avg * scenario$low.ed
        ) %>%
        select(Zip, Year, time_trend, median_income, low.ed,
               Total_narcan, rpopden, Non_violent, post_2023)
      
      # Make predictions
      pred <- predict(model, newdata = projected_data, se.fit = TRUE, type = "link")
      
      # Return predictions
      projected_data %>%
        mutate(
          scenario = scenario_name,
          predicted_me = exp(pred$fit),
          lower_ci = exp(pred$fit - 1.96 * pred$se.fit),
          upper_ci = exp(pred$fit + 1.96 * pred$se.fit)
        )
    })
  })
  
  return(all_predictions)
}

##Reminder of what model E is

model_e <- glm.nb(
  Total_ME ~ median_income + low.ed + Total_narcan + rpopden + 
    Non_violent + post_2023 + time_trend,
  data=zip_change,
  link=log
)


# Use this model for predictions
trend_predictions <- create_trending_predictions(model_e, years = 2025:2030, scenarios = scenarios)


# 1. TOTAL ME CASES ACROSS ALL ZIP CODES BY SCENARIO
# Aggregate predictions by year and scenario
scenario_totals <- trend_predictions %>%
  group_by(Year, scenario) %>%
  summarise(
    total_calls = sum(predicted_me, na.rm = TRUE),
    total_lower = sum(lower_ci, na.rm = TRUE),
    total_upper = sum(upper_ci, na.rm = TRUE),
    .groups = "drop"
  )

# Create historical totals for comparison (if available)
historical_totals <- zip_change %>%
  group_by(Year) %>%
  summarise(
    total_calls = sum(Total_ME, na.rm = TRUE),
    scenario = "historical",
    .groups = "drop"
  ) %>%
  mutate(total_lower = NA, total_upper = NA)

# Combine historical and predictions
combined_totals <- bind_rows(
  historical_totals,
  scenario_totals
)

# Plot 1: Total EMS Calls Timeline with trending model
pp3 <- ggplot(combined_totals, aes(x = Year, y = total_calls, color = scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(data = filter(combined_totals, scenario != "historical"),
              aes(ymin = total_lower, ymax = total_upper, fill = scenario), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("historical" = "black", 
                                "baseline" = "blue",
                                "optimistic" = "green",
                                "pessimistic"= "red")) +
  scale_fill_manual(values = c("baseline" = "blue",
                               "optimistic"= "green",
                               "pessimistic" ="red")) +
  labs(
    title = "Total OD Fatalities: Historical Data and Future Scenarios (2025-2030)",
    subtitle = "Shaded areas represent 95% confidence intervals",
    x = "Year",
    y = "Total OD Fatalities",
    color = "Scenario",
    fill = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2024.5, linetype = "dashed", alpha = 0.5) +
  scale_x_continuous(breaks = unique(combined_totals$Year)) +
  scale_y_continuous(limits = c(0, NA))

print(pp3)



# Check what the predictions look like
trend_predictions %>%
  group_by(Year, scenario) %>%
  summarise(
    total_ME = sum(predicted_me),
    mean_post_2023 = mean(post_2023),
    mean_time_trend = mean(time_trend)
  )
