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


####Creating a larger dataset for a predictive model
##Loading in the data from 2019-2024
ems21_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/EMS_2021.csv")
ems22_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/EMS_2022.csv")
ems23_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/EMS_2023.csv")
ME21_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/ME_2021.csv")
ME22_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/ME_2022.csv")
ME23_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/ME_2023.csv")

###filter and count by zip code before combining into a large dataset
ems21_data <- ems21_data %>%
  group_by(ZIP) %>%
  summarize(Total_ems = n()) %>%
  mutate(Year = 2021)

ems22_data <- ems22_data %>%
  group_by(ZIP) %>%
  summarize(Total_ems = n()) %>%
  mutate(Year = 2022)

ems23_data <- ems23_data %>%
  group_by(ZIP) %>%
  summarize(Total_ems = n()) %>%
  mutate(Year = 2023)

ems_data24 <- ems_data %>%
  mutate(Year = 2024) %>%
  rename(ZIP=Zip)

total_ems <- bind_rows(ems21_data, ems22_data, ems23_data, ems_data24)
total_ems <- total_ems[total_ems$ZIP %in% master_zips, ]


##do the same for ME
###filter and count by zip code before combining into a large dataset
ME21_data <- ME21_data %>%
  group_by(Zip) %>%
  summarize(Total_ME = n()) %>%
  mutate(Year = 2021)

ME22_data <- ME22_data %>%
  group_by(Zip) %>%
  summarize(Total_ME = n()) %>%
  mutate(Year = 2022)

ME23_data <- ME23_data %>%
  group_by(Zip) %>%
  summarize(Total_ME = n()) %>%
  mutate(Year = 2023)

ME24_data <- ME_data %>% 
  mutate(Year = 2024)

total_me <- bind_rows(ME21_data, ME22_data, ME23_data, ME24_data)
total_me <- total_me[total_me$Zip %in% master_zips, ]


##combine the two together and add in other zip code level information to this dataset
od_data <- total_me %>% 
  left_join(total_ems, by = c("Zip" = "ZIP", "Year" = "Year"))

od_data <- od_data[c("Zip", "Year", "Total_ME", "Total_ems")]

##combine with crime and policy map data
pm_data <- cleaned_data %>% select(-Total_ems, -Total_ME)


od_data <- od_data %>% left_join(pm_data, by = c("Zip" = "GeoID"), relationship = "many-to-one")


##Create columns for rates to be used in the predictive model 
od_data$EMS_rate <- round((od_data$Total_ems / od_data$rpopden *1000), 2)
od_data$ME_rate <- round((od_data$Total_ME / od_data$rpopden * 1000), 2)
od_data$Narcan_rate <- round((od_data$Total_narcan / od_data$rpopden * 1000), 2)
od_data$Violent_rate <- round((od_data$Violent / od_data$rpopden * 1000), 2)

names(od_data)[names(od_data) == "Non-violent"] <- "Non_violent"

od_data[is.na(od_data)] <- 0


# Get the 23 complete zip codes
complete_zips <- od_data %>%
  group_by(Zip) %>%
  summarise(n_years = n()) %>%
  filter(n_years == 4) %>%
  pull(Zip)

# Create multiple risk tiers
zip_risk_classification <- od_data %>%
  filter(Zip %in% complete_zips) %>%
  group_by(Zip) %>%
  summarise(
    zip_mean_income = mean(median_income, na.rm = TRUE),
    zip_mean_uninsured = mean(uninsured, na.rm = TRUE),
    zip_mean_low_ed = mean(low.ed, na.rm = TRUE),
    zip_mean_narcan = mean(Total_narcan, na.rm = TRUE),
    zip_mean_violent = mean(Violent, na.rm = TRUE),
    zip_mean_nonviolent = mean(Non_violent, na.rm = TRUE),
    zip_od_rate = mean(Total_ME, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Count risk factors
    risk_score = 
      (zip_mean_income < quantile(zip_mean_income, 0.33, na.rm = TRUE)) +
      (zip_mean_uninsured > quantile(zip_mean_uninsured, 0.67, na.rm = TRUE)) +
      (zip_mean_low_ed > quantile(zip_mean_low_ed, 0.67, na.rm = TRUE)) +
      (zip_mean_narcan > quantile(zip_mean_narcan, 0.67, na.rm = TRUE)) +
      (zip_mean_violent > quantile(zip_mean_violent, 0.67, na.rm = TRUE)) +
      (zip_mean_nonviolent > quantile(zip_mean_nonviolent, 0.67, na.rm = TRUE)) +
      (zip_od_rate > quantile(zip_od_rate, 0.67, na.rm = TRUE)),
    
    # Create risk tiers
    risk_tier = case_when(
      risk_score >= 5 ~ "Very High Risk",
      risk_score >= 3 ~ "High Risk",
      risk_score >= 2 ~ "Moderate Risk",
      TRUE ~ "Low Risk"
    ),
    risk_tier = factor(risk_tier, levels = c("Low Risk", "Moderate Risk", 
                                             "High Risk", "Very High Risk")),
    
    # Binary high risk flag
    high_risk_zip = risk_score >= 3
  )

# Check distribution
table(zip_risk_classification$risk_tier)
high_risk <- zip_change %>% 
  filter(risk_tier == "Very High Risk")


# Calculate the threshold across all zips
income_threshold <- quantile(zip_income_summary$zip_mean_income, 0.33, na.rm = TRUE)

# Now create zip_change with the high_risk_zip flag
# Apply the risk classification
zip_change <- od_data %>%
  filter(Zip %in% complete_zips) %>%
  arrange(Zip, Year) %>%
  left_join(zip_risk_classification %>% 
              select(Zip, high_risk_zip, risk_score, risk_tier), 
            by = "Zip") %>%
  group_by(Zip) %>%
  mutate(
    time_trend = Year - 2021,
    post_2023 = ifelse(Year >= 2024, 1, 0),
    baseline_avg = mean(Total_ems[Year %in% 2021:2023]),
    change_2024 = ifelse(Year == 2024,
                         (Total_ems - baseline_avg) / baseline_avg * 100, NA)
  ) %>% 
  ungroup()

# Visualize the risk distribution
zip_risk_classification %>%
  ggplot(aes(x = risk_tier)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Zip Codes by Risk Tier",
       x = "Risk Tier", y = "Number of Zip Codes") +
  theme_minimal()

# See which factors are most common in high-risk areas
zip_risk_classification %>%
  filter(high_risk_zip) %>%
  summarise(
    pct_low_income = mean(zip_mean_income < quantile(zip_mean_income, 0.33, na.rm = TRUE)),
    pct_high_crime = mean(zip_mean_violent > quantile(zip_mean_violent, 0.67, na.rm = TRUE)),
    pct_high_narcan = mean(zip_mean_narcan > quantile(zip_mean_narcan, 0.67, na.rm = TRUE))
  )

# Verify it worked
table(zip_change$high_risk_zip)

##creat zip-code level summary for 2024 analysis

zip_24_summary <- zip_change %>%
  filter(Year == 2024) %>%
  select(Zip, Total_ems, baseline_avg, change_2024,
         median_income, uninsured, low.ed, percent_nonwhite,
         Total_narcan, Total_ME, high_risk_zip) %>%
  arrange(desc(change_2024))

print("2024 changes by zip code Top 10:")
print(head(zip_24_summary[c("Zip", "change_2024", "baseline_avg", "Total_ems")], 10))


##Summary statistics 
change_stats <- zip_24_summary %>%
  summarise(
    mean_change = mean(change_2024, na.rm=TRUE),
    median_change = median(change_2024, na.rm=TRUE),
    sd_change = sd(change_2024, na.rm=TRUE),
    min_change = min(change_2024, na.rm=TRUE),
    max_change = max(change_2024, na.rm=TRUE),
    n_increased = sum(change_2024 > 20, na.rm=TRUE),
    n_spiked = sum(change_2024 > 100, na.rm=TRUE),
    n_decreased = sum(change_2024 < -10, na.rm=TRUE)
  )

print(change_stats)

# Visualize the distribution
ggplot(zip_24_summary, aes(x = change_2024)) +
  geom_histogram(bins = 8, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median(zip_24_summary$change_2024), 
             color = "blue", linetype = "dashed", size = 1) +
  labs(title = "2024 EMS Change Distribution Across 23 Zip Codes",
       subtitle = "Red line = no change, Blue line = median change",
       x = "Percent Change from 2021-2023 Average",
       y = "Number of Zip Codes") +
  theme_minimal()

##This is a bunch of code breaking down the changes seen in 2024 comparing to 2023 the model we are going to use starts at line:385
##comparing high-spike vs. low spike areas
zip_categories <- zip_24_summary %>%
  mutate(
    change_category = case_when(
      change_2024 < median(change_2024, na.rm=TRUE) ~ "High change",
      TRUE ~ "Low change"
    )
  )

##compare characteristics
comparison <- zip_categories %>%
  group_by(change_category) %>%
  summarise(
    n_zips = n(),
    mean_change=mean(change_2024),
    mean_income=mean(median_income, na.rm=TRUE),
    mean_uninsured=mean(uninsured, na.rm=TRUE),
    mean_poverty=mean(low.ed, na.rm=TRUE),
    mean_nonwhite=mean(percent_nonwhite, na.rm=TRUE),
    mean_narcan=mean(Total_narcan, na.rm=TRUE),
    mean_baseline=mean(baseline_avg, na.rm=TRUE),
    mean_ME=mean(Total_ME, na.rm=TRUE)
  )

print(comparison)

ppt_table <- comparison %>%
  mutate(
    # Format numbers for better presentation
    `ZIP Codes` = n_zips,
    `Mean Change` = round(mean_change, 1),
    `Median Income` = paste0("$", format(round(mean_income), big.mark = ",")),
    `Uninsured (%)` = paste0(round(mean_uninsured, 1), "%"),
    `Low Education (%)` = paste0(round(mean_poverty, 1), "%"),
    `Non-white (%)` = paste0(round(mean_nonwhite, 1), "%"),
    `Narcan Distribution` = round(mean_narcan, 1),
    `Baseline Average` = round(mean_baseline, 1),
    `Medical Examiner Cases` = round(mean_ME, 1)
  ) %>%
  select(change_category, `ZIP Codes`, `Mean Change`, `Median Income`, 
         `Uninsured (%)`, `Low Education (%)`, `Non-white (%)`, 
         `Narcan Distribution`, `Baseline Average`, `Medical Examiner Cases`) %>%
  rename(`Change Category` = change_category)

# Create the table with professional styling
final_table <- ppt_table %>%
  kable("html", escape = FALSE, align = c("l", rep("c", 9))) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14
  ) %>%
  row_spec(0, bold = TRUE, background = "#4472C4", color = "white") %>%
  row_spec(1, background = "#E7F3FF") %>%
  row_spec(2, background = "#FFF2E7") %>%
  column_spec(1, bold = TRUE, width = "2.5cm") %>%
  column_spec(2:10, width = "1.8cm") %>%
  add_header_above(c(" " = 1, "Demographics" = 4, "Health Indicators" = 3, "Outcomes" = 2),
                   background = "#2F5597", color = "white", bold = TRUE)

# Display the table
final_table


# Create top 10 ZIP codes table (assuming largest decreases = most negative values)
top10_zips <- zip_24_summary %>%
  arrange(change_2024) %>%  # Sort by change_2024 (most negative first)
  slice_head(n = 10) %>%    # Take top 10
  select(Zip, change_2024, median_income, uninsured, low.ed, 
         percent_nonwhite, Total_narcan, baseline_avg, Total_ME)

#Alternative layout with color-coded changes
top10_color_coded <- top10_zips %>%
  mutate(
    Rank = row_number(),
    `ZIP Code` = Zip,
    `Change 2024` = cell_spec(round(change_2024, 1), 
                              color = "white", 
                              background = "#DC3545",
                              bold = TRUE),
    `Median Income` = paste0("$", format(round(median_income), big.mark = ",")),
    `Uninsured (%)` = paste0(round(uninsured, 1), "%"),
    `Low Education (%)` = paste0(round(low.ed, 1), "%"),
    `Non-white (%)` = paste0(round(percent_nonwhite, 1), "%"),
    `Narcan Distribution` = round(Total_narcan, 1),
    `Baseline Average` = round(baseline_avg, 1),
    `Medical Examiner Cases` = round(Total_ME, 1)
  ) %>%
  select(Rank, `ZIP Code`, `Change 2024`, `Median Income`, 
         `Uninsured (%)`, `Low Education (%)`, `Non-white (%)`, 
         `Narcan Distribution`, `Baseline Average`, `Medical Examiner Cases`)

color_coded_table <- top10_color_coded %>%
  kable("html", escape = FALSE, align = c("c", "c", "c", rep("c", 7))) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE, background = "#4472C4", color = "white") %>%
  column_spec(1, bold = TRUE, width = "1cm", background = "#2F5597", color = "white") %>%
  column_spec(2, bold = TRUE, width = "1.5cm") %>%
  column_spec(4:10, width = "1.6cm") %>%
  add_header_above(c(" " = 1, " " = 1, " " = 1, "Demographics" = 4, "Health Indicators" = 3),
                   background = "#2F5597", color = "white", bold = TRUE)

color_coded_table



# Create bottom 10 ZIP codes table (smallest changes = least negative/most positive values)
bottom10_zips <- zip_24_summary %>%
  arrange(desc(change_2024)) %>%  # Sort by change_2024 (least negative/most positive first)
  slice_head(n = 10) %>%          # Take bottom 10 (smallest changes)
  select(Zip, change_2024, median_income, uninsured, low.ed, 
         percent_nonwhite, Total_narcan, baseline_avg, Total_ME)

####Alternative layout with color-coded changes (green theme)
bottom10_color_coded <- bottom10_zips %>%
  mutate(
    Rank = row_number(),
    `ZIP Code` = Zip,
    `Change 2024` = cell_spec(round(change_2024, 1), 
                              color = "white", 
                              background = "#28A745",  # Green background
                              bold = TRUE),
    `Median Income` = paste0("$", format(round(median_income), big.mark = ",")),
    `Uninsured (%)` = paste0(round(uninsured, 1), "%"),
    `Low Education (%)` = paste0(round(low.ed, 1), "%"),
    `Non-white (%)` = paste0(round(percent_nonwhite, 1), "%"),
    `Narcan Distribution` = round(Total_narcan, 1),
    `Baseline Average` = round(baseline_avg, 1),
    `Medical Examiner Cases` = round(Total_ME, 1)
  ) %>%
  select(Rank, `ZIP Code`, `Change 2024`, `Median Income`, 
         `Uninsured (%)`, `Low Education (%)`, `Non-white (%)`, 
         `Narcan Distribution`, `Baseline Average`, `Medical Examiner Cases`)

color_coded_bottom10_table <- bottom10_color_coded %>%
  kable("html", escape = FALSE, align = c("c", "c", "c", rep("c", 7))) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE, background = "#28A745", color = "white") %>%
  column_spec(1, bold = TRUE, width = "1cm", background = "#1E7E34", color = "white") %>%
  column_spec(2, bold = TRUE, width = "1.5cm") %>%
  column_spec(4:10, width = "1.6cm") %>%
  add_header_above(c(" " = 1, " " = 1, " " = 1, "Demographics" = 4, "Health Indicators" = 3),
                   background = "#1E7E34", color = "white", bold = TRUE)

color_coded_bottom10_table


# Create table for all ZIP codes with color-coded changes
all_zips_table <- zip_24_summary %>%
  arrange(change_2024) %>%  # Sort by change_2024 (most negative first)
  mutate(
    Rank = row_number(),
    # Create color-coded change values based on magnitude
    `Change 2024` = case_when(
      change_2024 <= quantile(change_2024, 0.25, na.rm = TRUE) ~ 
        cell_spec(round(change_2024, 1), 
                  color = "white", 
                  background = "#DC3545",  # Dark red for largest decreases
                  bold = TRUE),
      change_2024 <= quantile(change_2024, 0.5, na.rm = TRUE) ~ 
        cell_spec(round(change_2024, 1), 
                  color = "white", 
                  background = "#FD7E14",  # Orange for moderate decreases
                  bold = TRUE),
      change_2024 <= quantile(change_2024, 0.75, na.rm = TRUE) ~ 
        cell_spec(round(change_2024, 1), 
                  color = "white", 
                  background = "#FFC107",  # Yellow for smaller decreases
                  bold = TRUE),
      TRUE ~ 
        cell_spec(round(change_2024, 1), 
                  color = "white", 
                  background = "#28A745",  # Green for smallest changes/increases
                  bold = TRUE)
    ),
    `ZIP Code` = Zip,
    `Median Income` = paste0("$", format(round(median_income), big.mark = ",")),
    `Uninsured (%)` = paste0(round(uninsured, 1), "%"),
    `Low Education (%)` = paste0(round(low.ed, 1), "%"),
    `Non-white (%)` = paste0(round(percent_nonwhite, 1), "%"),
    `Narcan Distribution` = round(Total_narcan, 1),
    `Baseline Average` = round(baseline_avg, 1),
    `Medical Examiner Cases` = round(Total_ME, 1)
  ) %>%
  select(Rank, `ZIP Code`, `Change 2024`, `Median Income`, 
         `Uninsured (%)`, `Low Education (%)`, `Non-white (%)`, 
         `Narcan Distribution`, `Baseline Average`, `Medical Examiner Cases`)

# Create the full table with professional styling
full_color_coded_table <- all_zips_table %>%
  kable("html", escape = FALSE, align = c("c", "c", "c", rep("c", 7))) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11,
    fixed_thead = TRUE  # Keep header visible when scrolling
  ) %>%
  row_spec(0, bold = TRUE, background = "#4472C4", color = "white") %>%
  column_spec(1, bold = TRUE, width = "1cm", background = "#2F5597", color = "white") %>%
  column_spec(2, bold = TRUE, width = "1.5cm") %>%
  column_spec(4:10, width = "1.4cm") %>%
  add_header_above(c(" " = 1, " " = 1, " " = 1, "Demographics" = 4, "Health Indicators" = 3),
                   background = "#2F5597", color = "white", bold = TRUE) %>%
  add_footnote(c("Color Legend: Dark Red = Largest Decreases (Bottom 25%), Orange = Moderate Decreases (25-50%), Yellow = Smaller Decreases (50-75%), Green = Smallest Changes (Top 25%)"), 
               notation = "none", 
               escape = FALSE) %>%
  scroll_box(width = "100%", height = "600px")  # Add scrolling for long table

# Display the table
full_color_coded_table




###Model 1: standard negative binomial with year effects
model_1 <- glm.nb(
  Total_ems ~ Total_ME + median_age + median_income +
    low.ed +percent_nonwhite + uninsured + Total_narcan +
    factor(Year),
  data=zip_change,
  link=log
)

model_test <- glm.nb(
  Total_ems ~ Total_ME + median_age + median_income +
    low.ed +percent_nonwhite + uninsured + Total_narcan,
  data=zip_change,
  link=log
)



model_2 <- glm.nb(
  Total_ems ~ Total_ME + median_age + median_income +
    low.ed +percent_nonwhite + uninsured + Total_narcan +
    time_trend + post_2023,
  data=zip_change,
  link=log
)


model_3 <- glm.nb(
  Total_ems ~ Total_ME + median_age + median_income +
    low.ed + percent_nonwhite + uninsured + Total_narcan +
    time_trend + post_2023 +
    post_2023:high_risk_zip +
    Total_narcan:uninsured +
    Total_ME:post_2023,
  data=zip_change,
  link=log
)

models <- list(
  "Year Effects" = model_1,
  "Trend + Break" = model_2,
  "Geographic Interactions" = model_3
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

##Use the model of best fit from above to create predictions for 2025
create_2025_predictions <- function(model, scenarios = c("optimistic", "pessimistic", "continue")) {
  
  # Get zip code characteristics (keep by zip code, don't aggregate)
  zip_means <- zip_change %>%
    group_by(Zip) %>%
    summarise(across(c(Total_ME, median_age, median_income, low.ed,
                       percent_nonwhite, uninsured, Total_narcan, high_risk_zip),
                     ~ mean(.x, na.rm=TRUE)), .groups = "drop")
  
  # Create empty results dataframe
  all_predictions <- data.frame()
  
  for(scenario_name in scenarios) {
    
    # Create prediction data for this scenario (all zip codes)
    pred_data <- zip_means %>%
      mutate(
        Year = 2025,
        time_trend = 4,
        post_2023 = case_when(
          scenario_name == "optimistic" ~ 0,
          scenario_name == "pessimistic" ~ 1,
          scenario_name == "continue" ~ 1
        ),
        scenario = scenario_name
      )
    
    # Make predictions for all zip codes at once
    pred <- predict(model, newdata = pred_data, se.fit = TRUE)
    
    # Combine with zip codes and scenario info
    scenario_predictions <- pred_data %>%
      select(Zip, scenario) %>%
      mutate(
        prediction = exp(pred$fit),
        lower_ci = exp(pred$fit - 1.96 * pred$se.fit),
        upper_ci = exp(pred$fit + 1.96 * pred$se.fit)
      )
    
    # Add to results
    all_predictions <- rbind(all_predictions, scenario_predictions)
  }
  
  return(all_predictions)
}

summary(model_test)
coef(model_2)
prediction_2025 <- create_2025_predictions(model_2)

print(prediction_2025)

prediction_2025 <- prediction_2025 %>%
  mutate(Year = 2025,
         data_type="predicted") %>%
  rename(value=prediction) %>%
  select(Zip, Year, scenario, value, lower_ci, upper_ci, data_type)

# Aggregate historical data using Total_ems
historical_data <- zip_change %>%
  filter(Year %in% 2021:2024) %>%  # Only historical years
  group_by(Zip, Year) %>%
  summarise(
    value = sum(Total_ems, na.rm = TRUE),  # Using Total_ems for historical
    .groups = "drop"
  ) %>%
  mutate(
    scenario = "actual",
    lower_ci = NA,
    upper_ci = NA,
    data_type = "historical"
  ) %>%
  select(Zip, Year, scenario, value, lower_ci, upper_ci, data_type)


c_data <- bind_rows(historical_data, prediction_2025)

# Create summary for graphing (total across all zip codes)
combined_summary <- c_data %>%
  group_by(Year, scenario, data_type) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),
    total_lower_ci = if_else(all(is.na(lower_ci)), NA_real_, sum(lower_ci, na.rm = TRUE)),
    total_upper_ci = if_else(all(is.na(upper_ci)), NA_real_, sum(upper_ci, na.rm = TRUE)),
    .groups = "drop"
  )


connecting_data <- filter(combined_summary, scenario == "actual", Year == 2024) %>%
  select(-scenario) %>%
  crossing(scenario = c("optimistic", "pessimistic", "continue"))

prediction_lines <- combined_summary %>% 
  filter(scenario != "actual") %>%
  bind_rows(connecting_data)


ggplot(combined_summary, aes(x = Year, y = total_value)) +
  # Historical line (black)
  geom_line(data = filter(combined_summary, scenario == "actual"), 
            color = "black", size = 1.2) +
  geom_point(data = filter(combined_summary, scenario == "actual"), 
             color = "black", size = 3) +
  # Prediction lines from 2024 to 2025
  geom_line(data = prediction_lines,
            aes(color = scenario), size = 1.2, linetype = "dashed") +
  geom_point(data = filter(combined_summary, scenario != "actual"), 
             aes(color = scenario), size = 3) +
  labs(title = "EMS Calls: Historical Trend and 2025 Scenario Projections") +
  scale_color_manual(values = c("optimistic" = "green", 
                                "pessimistic" = "red", 
                                "continue" = "blue")) +
  scale_y_continuous(limits = c(0,NA)) +
  theme_minimal()


##Testing to see model we chose to see how well it will predict the 2024 data against the actually numbers we have for that year

train_data <- zip_change %>% filter(Year != 2024)
test_data <- zip_change %>% filter(Year == 2024)

validation_model <- glm.nb(
  Total_ems ~ Total_ME + median_age + median_income +
    low.ed +percent_nonwhite + uninsured + Total_narcan +
    time_trend + post_2023,
  data=train_data,
  link=log
)

summary(validation_model)


test_data_pred <- test_data %>%
  mutate(post_2023 = 1)

pred_24 <- predict(validation_model, newdata=test_data_pred, type = "response")

validation_results <- data.frame(
  Zip = test_data$Zip,
  actual_2024 = test_data$Total_ems,
  predicted_24 <- pred_24,
  error = test_data$Total_ems - pred_24,
  pct_error = abs(test_data$Total_ems-pred_24)/test_data$Total_ems*100
)


print("VALIDATION RESULTS - TOP 10 ERRORS:")
print(head(validation_results[order(-abs(validation_results$error)), ], 10))

mean_abs_pct_error <- mean(validation_results$pct_error, na.rm = TRUE)
print(paste("Mean Absolute Percentage Error:", round(mean_abs_pct_error, 1), "%"))


pred_graph <- ggplot(validation_results, aes(x = predicted_24, y = actual_2024)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Values",
       x = "Predicted 2024", y = "Actual 2024") +
  theme_minimal()
pred_graph

