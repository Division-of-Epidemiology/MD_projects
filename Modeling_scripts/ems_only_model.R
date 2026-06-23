##EMS ONLY MODELING
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
ems_complete <- glm.nb(
  Total_ems ~ median_age + median_income + low.ed + uninsured + 
    Total_narcan + rpopden + Non_violent + post_2023 + high_risk_zip +
    Total_narcan:uninsured, 
  data=zip_change,
  link=log
)


step_model_ems <- stepAIC(ems_complete, direction = "both")




###Model 1: standard negative binomial with year effects
model_a <- glm.nb(
  Total_ems ~ median_age + median_income + low.ed + uninsured + 
    Total_narcan + rpopden + Non_violent + post_2023 + high_risk_zip +
    factor(Year),
  data=zip_change,
  link=log
)


model_b <- glm.nb(
  Total_ems ~ median_age + median_income + low.ed + uninsured + 
    Total_narcan + rpopden + Non_violent + post_2023 + high_risk_zip + time_trend,
  data=zip_change,
  link=log
)


model_c <- glm.nb(
  Total_ems ~ median_age + median_income + low.ed + uninsured + 
    Total_narcan + rpopden + Non_violent + post_2023 + high_risk_zip +
    high_risk_zip:post_2023 +
    Total_narcan:uninsured, 
  data=zip_change,
  link=log
)

summary(model_b)

models <- list(
  "Year Effects" = model_a,
  "Trend + Break" = model_b,
  "Geographic Interactions" = model_c
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


summary(model_d)

model_d <- glm.nb(Total_ems ~ median_age + median_income + Non_violent +
         uninsured + Total_narcan + post_2023 +
         Total_narcan:uninsured, 
         data = zip_change, link = log)

##model C is the best one so we will continue with that one for the predictions!

# Time-based split (most realistic for your use case)
train_data <- zip_change[zip_change$Year %in% c(2021, 2022, 2023), ]
test_data <- zip_change[zip_change$Year == 2024, ]

# Refit model on training data only
model_validation_ems <- glm.nb(Total_ems ~ median_age + median_income + low.ed + uninsured + 
                                 Total_narcan + rpopden + Non_violent + post_2023 + high_risk_zip +
                                 high_risk_zip:post_2023 +
                                 Total_narcan:uninsured,
  data=train_data,
  link=log)

# Test predictions on 2024 data
test_predictions <- predict(model_validation_ems, newdata = test_data, se.fit = TRUE)

# Compare predicted vs actual
validation_results <- test_data %>%
  mutate(
    predicted = exp(test_predictions$fit),
    actual = Total_ems,
    error = actual - predicted,
    abs_error = abs(error),
    pct_error = abs(error) / actual * 100
  )

# Summary statistics
mean(validation_results$abs_error, na.rm = TRUE)  # Mean absolute error
mean(validation_results$pct_error, na.rm = TRUE)  # Mean percentage error
r<- cor(validation_results$predicted, validation_results$actual) # Correlation
r_squared <- r^2
mae <- mean(validation_results$abs_error, na.rm = TRUE)
mpe <- mean(validation_results$pct_error, na.rm = TRUE)

# Test significance
cor_test <- cor.test(validation_results$predicted, validation_results$actual)

cat("Correlation (r):", r, "\n")
cat("R-squared (r²):", r_squared, "\n")
cat("P-value:", cor_test$p.value, "\n")
cat("95% CI:", cor_test$conf.int, "\n")
cat("MAE:", mae, "\n")
cat("MPE:", mpe, "\n")

# Check the scale of your EMS calls to contextualize the error
summary(validation_results$actual)  # What's the typical range?
median(validation_results$actual)   # Median actual calls

# See if errors are consistent across prediction ranges
plot(validation_results$actual, validation_results$abs_error)


create_scenario_predictions <- function(model, years = 2025:2030) {
  
  # Base year data - preserve variable types
  base_data <- zip_change %>%
    filter(Year == max(Year)) %>%
    group_by(Zip) %>%
    summarise(
      median_age = mean(median_age, na.rm = TRUE),
      median_income = mean(median_income, na.rm = TRUE),
      low.ed = mean(low.ed, na.rm = TRUE),
      uninsured = mean(uninsured, na.rm = TRUE),
      Total_narcan = mean(Total_narcan, na.rm = TRUE),
      rpopden = mean(rpopden, na.rm = TRUE),
      Non_violent = mean(Non_violent, na.rm = TRUE),
      post_2023 = mean(as.numeric(post_2023), na.rm = TRUE),
      high_risk_zip = as.logical(round(mean(as.numeric(high_risk_zip), na.rm = TRUE))),
      .groups = "drop"
    )
  
  scenarios <- list(
    optimistic = list(
      Total_narcan = 0.90,      # 10% decrease in narcan
      uninsured = 0.95          # 5% decrease in uninsured
    ),
    pessimistic = list(
      Total_narcan = 1.10,      # 10% increase in narcan 
      uninsured = 1.10          # 10% increase in uninsured
    ),
    baseline = list(
      Total_narcan = 1.00,      # No change
      uninsured = 1.00
    )
  )
  
  all_predictions <- map_dfr(names(scenarios), function(scenario_name) {
    scenario_data <- base_data %>%
      mutate(
        Total_narcan = Total_narcan * scenarios[[scenario_name]]$Total_narcan,
        uninsured = uninsured * scenarios[[scenario_name]]$uninsured
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
          predicted_ems = exp(pred$fit),
          lower_ci = exp(pred$fit - 1.96 * pred$se.fit),
          upper_ci = exp(pred$fit + 1.96 * pred$se.fit)
        )
    })
  })
  
  return(all_predictions)
}

scenario_predictions_ems <- create_scenario_predictions(model_b)
####trying to combine above with time component
scenarios <- list(
  optimistic = list(
    median_income = 1.10,
    uninsured = 0.95,
    median_age = 0.98,
    Total_narcan = 1.10,
    low.ed = 0.95,
    rpopden = 1.10,
    Non_violent = 0.90
  ),
  pessimistic = list(
    median_income = 0.50,
    uninsured = 1.50,
    median_age = 1.02,
    Total_narcan = 0.90,
    low.ed = 1.15,
    rpopden = 0.90,
    Non_violent = 1.50
  ),
  baseline = list(
    median_income = 1.00,
    uninsured = 1.00,
    median_age = 1.00,
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
      uninsured = 1.00,
      median_age = 1.00,
      Total_narcan = 1.00,
      low.ed = 1.00,
      rpopden = 1.00,
      Non_violent = 1.00
    ))
  }
  
  # Get high_risk_zip status for each zip
  zip_risk_status <- zip_change %>%
    group_by(Zip) %>%
    summarise(high_risk_zip = first(high_risk_zip), .groups = "drop")
  
  # Fit trend models for each zip code and variable
  zip_trends <- zip_change %>%
    group_by(Zip) %>%
    summarise(
      income_slope = tryCatch(coef(lm(median_income ~ Year))[2], error = function(e) 0),
      income_intercept = tryCatch(coef(lm(median_income ~ Year))[1], error = function(e) mean(median_income, na.rm = TRUE)),
      uninsured_slope = tryCatch(coef(lm(uninsured ~ Year))[2], error = function(e) 0),
      uninsured_intercept = tryCatch(coef(lm(uninsured ~ Year))[1], error = function(e) mean(uninsured, na.rm = TRUE)),
      age_slope = tryCatch(coef(lm(median_age ~ Year))[2], error = function(e) 0),
      age_intercept = tryCatch(coef(lm(median_age ~ Year))[1], error = function(e) mean(median_age, na.rm = TRUE)),
      narcan_slope = tryCatch(coef(lm(Total_narcan ~ Year))[2], error = function(e) 0),
      narcan_intercept = tryCatch(coef(lm(Total_narcan ~ Year))[1], error = function(e) mean(Total_narcan, na.rm = TRUE)),
      pop_slope = tryCatch(coef(lm(rpopden ~ Year))[2], error = function(e) 0),
      pop_intercept = tryCatch(coef(lm(rpopden ~ Year))[1], error = function(e) mean(rpopden, na.rm = TRUE)),
      low_ed_avg = mean(low.ed, na.rm = TRUE),
      crime_slope = tryCatch(coef(lm(Non_violent ~ Year))[2], error = function(e) 0),
      crime_intercept = tryCatch(coef(lm(Non_violent ~ Year))[1], error = function(e) mean(Non_violent, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    left_join(zip_risk_status, by = "Zip")
  
  # Loop over scenarios then years
  all_predictions <- map_dfr(names(scenarios), function(scenario_name) {
    scenario <- scenarios[[scenario_name]]
    
    map_dfr(years, function(year) {
      # Project predictor values, then apply scenario multipliers
      projected_data <- zip_trends %>%
        mutate(
          Year = year,
          time_trend = year - 2021,  # Add time_trend
          post_2023 = year - 2023,   # Continuous: 2 for 2025, 3 for 2026, etc.
          median_income = pmax(0, income_intercept + income_slope * year) * scenario$median_income,
          uninsured = pmax(0, pmin(100, uninsured_intercept + uninsured_slope * year)) * scenario$uninsured,
          median_age = pmax(18, pmin(100, age_intercept + age_slope * year)) * scenario$median_age,
          Total_narcan = pmax(0, narcan_intercept + narcan_slope * year) * scenario$Total_narcan,
          rpopden = pmax(0, pop_intercept + pop_slope * year) * scenario$rpopden,
          Non_violent = pmax(0, crime_intercept + crime_slope * year) * scenario$Non_violent,
          low.ed = low_ed_avg * scenario$low.ed
        ) %>%
        select(Zip, Year, time_trend, median_income, median_age, low.ed, uninsured, 
               Total_narcan, rpopden, Non_violent, post_2023, high_risk_zip)
      
      # Make predictions
      pred <- predict(model, newdata = projected_data, se.fit = TRUE)
      
      # Return predictions
      projected_data %>%
        mutate(
          scenario = scenario_name,
          predicted_ems = exp(pred$fit),
          lower_ci = exp(pred$fit - 1.96 * pred$se.fit),
          upper_ci = exp(pred$fit + 1.96 * pred$se.fit)
        )
    })
  })
  
  return(all_predictions)
}

# Refit model with continuous post_2023
zip_change_continuous <- zip_change %>%
  mutate(post_2023 = pmax(0, Year - 2023))  # 0 for years <= 2023, then 1, 2, 3...

model_b_continuous <- glm(Total_ME ~ median_age + median_income + low.ed + uninsured + 
                            Total_narcan + rpopden + Non_violent + post_2023 + high_risk_zip +
                            time_trend,
                          data = zip_change_continuous,
                          family = poisson(link = "log"))

summary(model_b_continuous)



# Use this model for predictions
trend_predictions <- create_trending_predictions(model_b, years = 2025:2030, scenarios = scenarios)


# 1. TOTAL EMS CALLS ACROSS ALL ZIP CODES BY SCENARIO
# Aggregate predictions by year and scenario
scenario_totals <- trend_predictions %>%
  group_by(Year, scenario) %>%
  summarise(
    total_calls = sum(predicted_ems, na.rm = TRUE),
    total_lower = sum(lower_ci, na.rm = TRUE),
    total_upper = sum(upper_ci, na.rm = TRUE),
    .groups = "drop"
  )

# Create historical totals for comparison (if available)
historical_totals <- zip_change %>%
  group_by(Year) %>%
  summarise(
    total_calls = sum(Total_ems, na.rm = TRUE),
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
pp2 <- ggplot(combined_totals, aes(x = Year, y = total_calls, color = scenario)) +
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
    title = "Total EMS Calls: Historical Data and Future Scenarios (2025-2030)",
    subtitle = "Shaded areas represent 95% confidence intervals",
    x = "Year",
    y = "Total EMS Calls",
    color = "Scenario",
    fill = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = 2024.5, linetype = "dashed", alpha = 0.5) +
  scale_x_continuous(breaks = unique(combined_totals$Year)) +
  scale_y_continuous(limits = c(0, NA))

plot(pp2)

summary(model_b_continuous)

# Check what the predictions look like
trend_predictions %>%
  group_by(Year, scenario) %>%
  summarise(
    total_ems = sum(predicted_ems),
    mean_post_2023 = mean(post_2023),
    mean_time_trend = mean(time_trend)
  )


##Graphs for model results 
# Flag top 5 worst-predicted zips
validation_results <- validation_results %>%
  mutate(error_size = abs(actual - predicted))

ggplot(validation_results, aes(x=predicted, y=actual, color=error_size)) +
  geom_point(alpha=0.8, size=3) +
  geom_abline(slope=1, intercept=0, color="black", linewidth=0.8, linetype="dashed") +
  scale_x_log10(labels=scales::comma) +
  scale_y_log10(labels=scales::comma) +
  scale_color_gradientn(
    colors = c("#2C7BB6", "#FFFFBF", "#D7191C"),
    name = "Absolute\nError"
  ) +
  labs(
    title = "Model 1: Predicted vs. Actual EMS Calls",
    subtitle = "Color = absolute error magnitude",
    x = "Predicted EMS Calls",
    y = "Actual EMS Calls"
  ) +
  theme_minimal(base_size=13) +
  theme(
    plot.title = element_text(face="bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )