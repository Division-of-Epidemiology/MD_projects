##looking at changes across time rather than just what the change in 2024 was
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

total_change <- od_data %>%
  filter(Zip %in% complete_zips) %>%
  arrange(Zip, Year) %>% 
  group_by(Zip) %>%
  mutate(
    time_trend = Year - 2021,
    post_2023 = ifelse(Year >= 2024, 1,0),
    baseline_avg = mean(Total_ems[Year %in% 2021:2024]),
    baseline_ME = mean(Total_ME[Year %in% 2021:2024]),
    
    #year over year changes for EMS
    change_2021_2022 = ifelse(Year == 2022,
                              (Total_ems - lag(Total_ems, order_by = Year))/lag(Total_ems, order_by = Year)*100, 
                              NA),
    change_2022_2023 = ifelse(Year == 2023,
                              (Total_ems - lag(Total_ems, order_by = Year))/lag(Total_ems, order_by = Year)*100, 
                              NA),
    change_2023_2024 = ifelse(Year == 2024,
                              (Total_ems - lag(Total_ems, order_by = Year))/lag(Total_ems, order_by = Year)*100, 
                              NA),
    change_2021_2022ME = ifelse(Year == 2022,
                              (Total_ME - lag(Total_ME, order_by = Year))/lag(Total_ME, order_by = Year)*100, 
                              NA),
    change_2022_2023ME = ifelse(Year == 2023,
                              (Total_ME - lag(Total_ME, order_by = Year))/lag(Total_ME, order_by = Year)*100, 
                              NA),
    change_2023_2024ME = ifelse(Year == 2024,
                              (Total_ME - lag(Total_ME, order_by = Year))/lag(Total_ME, order_by = Year)*100, 
                              NA),
    zip_mean_income = mean(median_income),
    zip_mean_uninsured = mean(uninsured)
  ) %>% ungroup()


##Time trend anaylsis for the change over time
zip_time_analysis <- od_data %>%
  filter(Zip %in% complete_zips) %>%
  arrange(Zip, Year) %>%
  group_by(Zip) %>%
  mutate(
    time_trend = Year - 2021,
    years_from_start = row_number() - 1,
    baseline_2021 = first(Total_ems),
    pct_change_2021 = (Total_ems - baseline_2021)/baseline_2021*100,
    baseline_21ME = first(Total_ME),
    pct_change_21ME = (Total_ME - baseline_21ME)/baseline_21ME*100,
    #year over year changes
    yoy_change = (Total_ems - lag(Total_ems))/lag(Total_ems)*100,
    yoy_abs_change = Total_ems - lag(Total_ems),
    yoy_changeME = (Total_ME - lag(Total_ME))/lag(Total_ME)*100,
    yoy_abs_changeME = Total_ME - lag(Total_ME),
    #moving avg
    ma_2yr = (Total_ems + lag(Total_ems))/2,
    cumulative_change = Total_ems - baseline_2021,
    ma_2yr_ME = (Total_ME + lag(Total_ME))/2,
    cumulative_changeME = Total_ME - baseline_21ME
  ) %>% ungroup()


##overall trends per year
overall_trends <- zip_time_analysis %>%
  group_by(Year) %>%
  summarise(
    n_zips = n(),
    mean_ems = mean(Total_ems, na.rm = TRUE),
    median_ems = median(Total_ems, na.rm = TRUE),
    sd_ems = sd(Total_ems, na.rm = TRUE),
    total_ems = sum(Total_ems, na.rm = TRUE),
    mean_yoy_change = mean(yoy_change, na.rm = TRUE),
    median_yoy_change = median(yoy_change, na.rm = TRUE),
    pct_increasing = mean(yoy_change > 0, na.rm = TRUE) * 100,
    .groups = 'drop'
  )

##zip code trajectories
zip_trajectories <- zip_time_analysis %>%
  group_by(Zip) %>%
  summarise(
    baseline_ems = first(Total_ems),
    final_ems = last(Total_ems),
    total_change = final_ems - baseline_ems,
    total_pct_change = (final_ems-baseline_ems)/baseline_ems*100,
    
    ##trend analysis
    mean_yoy_change = mean(yoy_change, na.rm=TRUE),
    volatility = sd(yoy_change, na.rm=TRUE),
    
    #pattern classification
    consistently_increasing = all(yoy_change >= 0, na.rm = TRUE),
    consistently_decreasing = all(yoy_change <= 0, na.rm = TRUE),
    peak_year = Year[which.max(Total_ems)],
    trough_year = Year[which.min(Total_ems)],
    
    .groups = 'drop'
  ) %>%
  mutate(
    trajectory_type=case_when(
      consistently_increasing ~ "Consistent Increase",
      consistently_decreasing ~ "Consistent Decrease", 
      total_pct_change > 10 ~ "Net Increase",
      total_pct_change < -10 ~ "Net Decrease",
      TRUE ~ "Stable/Mixed"
    )
  )


#looking at different time periods and comparing them
pre_post_analysis <- zip_time_analysis %>%
  mutate(period=ifelse(Year<=2023, "Pre-2024", "2024+")) %>%
  group_by(Zip, period) %>%
  summarise(
    mean_ems=mean(Total_ems, na.rm=TRUE),
    n_years=n(),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from=period, values_from = c(mean_ems, n_years)) %>%
  mutate(
    period_change = `mean_ems_2024+` - `mean_ems_Pre-2024`,
    period_pct_change = period_change/`mean_ems_Pre-2024`*100
  )

#statistical testing for each zip code when in comes to the change and linear trend for each one
zip_trend_models <- zip_time_analysis %>%
  group_by(Zip) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(Total_ems ~ time_trend, data = .x)),
    trend_results = map(model, tidy)
  ) %>%
  unnest(trend_results) %>%
  filter(term == "time_trend") %>%
  select(Zip, slope = estimate, p_value = p.value, std_error=std.error) %>%
  mutate(
    sig_trend = p_value < 0.05,
    trend_direction = case_when(
      sig_trend & slope > 0 ~ "Significant Increase",
      sig_trend & slope < 0 ~ "Significant Decrease", 
      TRUE ~ "No Significant Trend"
    )
  )



##All the graphs
plot_1 <- zip_time_analysis %>%
  filter(!is.na(yoy_change)) %>%
  ggplot(aes(x = factor(Year), y = yoy_change)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of Year-over-Year Changes",
       subtitle = "Boxplots showing change variability by year",
       x = "Year", y = "Year-over-Year % Change") +
  theme_minimal()

plot_1

plot_2 <- overall_trends %>%
  select(Year, mean_ems, median_ems) %>%
  pivot_longer(cols = c(mean_ems, median_ems), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Year, y = value, color = metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("mean_ems" = "blue", "median_ems" = "red"),
                     labels = c("Mean", "Median"),
                     name = "Statistic") +
  labs(title = "Overall EMS Call Trends",
       subtitle = "Mean vs Median across all zip codes",
       x = "Year", y = "EMS Calls") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,NA))+
  theme(legend.position = "bottom")

plot_2

heatmap_data <- zip_time_analysis %>%
  filter(!is.na(yoy_change)) %>%
  select(Zip, Year, yoy_change) %>%
  mutate(Zip = factor(Zip))

plot_3 <- ggplot(heatmap_data, aes(x = factor(Year), y = Zip, fill = yoy_change)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, name = "YoY\nChange %") +
  labs(title = "Year-over-Year Changes by Zip Code",
       subtitle = "Heatmap showing annual change patterns",
       x = "Year", y = "Zip Code") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

plot_3


plot_4 <- zip_trend_models %>%
  ggplot(aes(x = trend_direction, fill = trend_direction)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("Significant Increase" = "green",
                               "Significant Decrease" = "red", 
                               "No Significant Trend" = "gray")) +
  labs(title = "Statistical Trend Analysis Results",
       subtitle = "Number of zip codes by trend significance",
       x = "Trend Direction", y = "Number of Zip Codes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

plot_4

plot_5 <- zip_trend_models %>%
  ggplot(aes(x = trend_direction, fill = trend_direction)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("Significant Increase" = "green",
                               "Significant Decrease" = "red", 
                               "No Significant Trend" = "gray")) +
  labs(title = "Statistical Trend Analysis Results",
       subtitle = "Number of zip codes by trend significance",
       x = "Trend Direction", y = "Number of Zip Codes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

plot_5



##looking closer at the 6 zip codes that had significant decreases in 2024
# Get zip codes with significant decreases
significant_decreases <- zip_trend_models %>%
  filter(trend_direction == "Significant Decrease") %>%
  pull(Zip)

# Add significance flag to main dataset
zip_time_viz <- zip_time_analysis %>%
  mutate(
    significance_status = case_when(
      Zip %in% significant_decreases ~ "Significant Decrease",
      Zip %in% filter(zip_trend_models, trend_direction == "Significant Increase")$Zip ~ "Significant Increase",
      TRUE ~ "No Significant Trend"
    )
  )


zip_time_viz$Zip <- as.factor(zip_time_viz$Zip)
plot_6 <- ggplot(zip_time_viz, aes(x = Year, y = Total_ems, group = Zip)) + 
  geom_line(data = filter(zip_time_viz, significance_status == "No Significant Trend"), 
            color = "lightgray", alpha = 0.5, size = 0.5) + 
  geom_line(data = filter(zip_time_viz, significance_status == "Significant Increase"), 
            color = "lightblue", alpha = 0.7, size = 0.8) + 
  geom_line(data = filter(zip_time_viz, significance_status == "Significant Decrease"), 
            aes(color = Zip), alpha = 0.9, size = 1.2) +
  scale_color_manual(values = rep("red", length(unique(filter(zip_time_viz, significance_status == "Significant Decrease")$Zip))),
                     name = "ZIP Codes with\nSignificant Decrease") +
  labs(title = "EMS Trajectories: Highlighting Significant Decreases", 
       subtitle = "Red lines = Significant decreases, Blue = Increases, Gray = No trend", 
       x = "Year", 
       y = "Total EMS calls") + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"))

plot_6


####Tracking risk level over time
# Calculate risk classification for each year
yearly_risk_classification <- od_data %>%
  filter(Zip %in% complete_zips) %>%
  group_by(Zip, Year) %>%
  summarise(
    zip_income = mean(median_income, na.rm = TRUE),
    zip_uninsured = mean(uninsured, na.rm = TRUE),
    zip_low_ed = mean(low.ed, na.rm = TRUE),
    zip_narcan = mean(Total_narcan, na.rm = TRUE),
    zip_violent = mean(Violent, na.rm = TRUE),
    zip_nonviolent = mean(Non_violent, na.rm = TRUE),
    zip_od_count = sum(Total_ME, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Year) %>%  # Calculate thresholds within each year
  mutate(
    # Count risk factors using year-specific thresholds
    risk_score = 
      (zip_income < quantile(zip_income, 0.33, na.rm = TRUE)) +
      (zip_uninsured > quantile(zip_uninsured, 0.67, na.rm = TRUE)) +
      (zip_low_ed > quantile(zip_low_ed, 0.67, na.rm = TRUE)) +
      (zip_narcan > quantile(zip_narcan, 0.67, na.rm = TRUE)) +
      (zip_violent > quantile(zip_violent, 0.67, na.rm = TRUE)) +
      (zip_nonviolent > quantile(zip_nonviolent, 0.67, na.rm = TRUE)) +
      (zip_od_count > quantile(zip_od_count, 0.67, na.rm = TRUE)),
    
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
yearly_risk_classification %>%
  count(Year, risk_tier) %>%
  pivot_wider(names_from = risk_tier, values_from = n)

# Identify zip codes that changed risk status
risk_transitions <- yearly_risk_classification %>%
  arrange(Zip, Year) %>%
  group_by(Zip) %>%
  mutate(
    previous_risk_tier = lag(risk_tier),
    risk_tier_change = risk_tier != previous_risk_tier,
    previous_high_risk = lag(high_risk_zip),
    changed_status = high_risk_zip != previous_high_risk
  ) %>%
  ungroup()

# Summary of transitions
risk_transitions %>%
  filter(!is.na(changed_status), changed_status == TRUE) %>%
  count(Year, previous_high_risk, high_risk_zip) %>%
  mutate(
    transition_type = case_when(
      previous_high_risk == FALSE & high_risk_zip == TRUE ~ "Became High Risk",
      previous_high_risk == TRUE & high_risk_zip == FALSE ~ "Improved to Low Risk"
    )
  )

# Which zip codes moved in/out of high risk?
movers <- risk_transitions %>%
  filter(!is.na(changed_status), changed_status == TRUE) %>%
  select(Zip, Year, previous_risk_tier, risk_tier, risk_score, 
         zip_income, zip_narcan, zip_violent, zip_od_count)

head(movers, 20)

# Sankey-style visualization showing transitions
library(ggalluvial)

# Create flow data for consecutive years
flow_data <- yearly_risk_classification %>%
  filter(Year %in% c(2021, 2022, 2023, 2024)) %>%
  select(Zip, Year, risk_tier) %>%
  pivot_wider(names_from = Year, values_from = risk_tier, names_prefix = "Year_")

# Count flows between years
flow_summary <- flow_data %>%
  count(Year_2021, Year_2022, Year_2023, Year_2024)

# Alluvial plot
yearly_risk_classification %>%
  filter(Year %in% c(2021, 2022, 2023, 2024)) %>%
  ggplot(aes(x = Year, stratum = risk_tier, alluvium = Zip, 
             fill = risk_tier, label = risk_tier)) +
  geom_flow(stat = "alluvium") +
  geom_stratum() +
  scale_fill_manual(values = c("Low Risk" = "green3", 
                               "Moderate Risk" = "yellow2",
                               "High Risk" = "orange2",
                               "Very High Risk" = "red2")) +
  labs(title = "Zip Code Risk Tier Transitions Over Time",
       y = "Number of Zip Codes") +
  theme_minimal()

# Line plot showing risk score trends for specific zips  (only one zip code right now)
high_movers <- risk_transitions %>%
  group_by(Zip) %>%
  summarise(
    n_changes = sum(changed_status, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_changes >= 2) %>%  # Zips that changed multiple times
  pull(Zip)

yearly_risk_classification %>%
  filter(Zip %in% high_movers) %>%
  ggplot(aes(x = Year, y = risk_score, group = Zip, color = Zip)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
  labs(title = "Risk Score Trajectories for Volatile Zip Codes",
       subtitle = "Dashed line = High Risk threshold",
       y = "Risk Score (0-7)") +
  theme_minimal()