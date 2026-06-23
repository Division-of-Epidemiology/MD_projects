##Building a regression model to see how Narcan affects EMS calls
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)



Narcan_OT <- data.frame(Year = c(2016:2025),
  Bystander_total = c(13, 40, 46, 74, 188, 172, 204, 178, 128,148),
  Narcan_total= c(1568, 1930, 2330, 2861, 3531, 3184, 3341, 2871, 2150, 1749),
  EMS_total = c(3074, 3540, 3903, 4397, 5899, 5733, 5707, 5601, 4536, 3869),
  Fentaly_percent = c(21.0, 31.8, 51.0, 62.4, 77.0, 75.6, 77.6, 79.2, 69.7, 65.3))

Narcan_OT
#######
plot(Narcan_OT$Year, Narcan_OT$EMS_total)
plot(Narcan_OT$Year, Narcan_OT$Fentaly_percent)

new_model <- lm(EMS_total ~ Bystander_total + Narcan_total + Fentaly_percent + Year, data=Narcan_OT)
summary(new_model)

qmodel <- lm(EMS_total ~ Bystander_total + Narcan_total + Fentaly_percent, data=Narcan_OT)
summary(qmodel)
##Just looking at EMS and bystander narcan
bmodel <- lm(EMS_total ~ Bystander_total, data=Narcan_OT)
summary(bmodel)

cmodel <- lm(EMS_total ~ Fentaly_percent, data=Narcan_OT)
summary(cmodel)

wmodel <- lm(EMS_total ~ Bystander_total + Narcan_total, data=Narcan_OT)
summary(wmodel)

##Seeing if time plays a role

diff_data <- data.frame(
  Year = 2017:2025,
  dEMS = diff(Narcan_OT$EMS_total),
  dBystander = diff(Narcan_OT$Bystander_total))


model_diff <- lm(dEMS ~ dBystander, data=diff_data)
summary(model_diff)

##Seeing if fentanyl plays a role
fent_model <- lm(EMS_total ~ Fentaly_percent, data=Narcan_OT)
summary(fent_model)



FB_model <- lm(EMS_total ~ Fentaly_percent + Bystander_total, data=Narcan_OT)
summary(FB_model)


cor(Narcan_OT[, c("Bystander_total", "Narcan_total", "Fentaly_percent")])

####Lagged-model
Narcan_OT$Lag_Bystander <- dplyr::lag(Narcan_OT$Bystander_total, 1)
Narcan_OT$Lag_Fentanyl <- dplyr::lag(Narcan_OT$Fentaly_percent, 1)

lagged_data <- na.omit(Narcan_OT)

lagged_model <- lm(EMS_total ~ Lag_Bystander * Lag_Fentanyl + Year, data=lagged_data)
summary(lagged_model)



##Poisson modeling
model_poisson <- glm(EMS_total ~ Fentaly_percent, family = poisson(), data = Narcan_OT)
summary(model_poisson)

model_qp <- glm(EMS_total ~ Fentaly_percent, family = quasipoisson(), data = Narcan_OT)
summary(model_qp)


##using more count data so breaking it down by months overtime 
ncan_track$Month <- format(as.Date(ncan_track$Incident_Date), "%B")

# 1. Prepare total incident calls per month
total_incidents <- ncan_track %>%
  filter(PD_jail == 0, MMWR_Year != 2015) %>%
  mutate(Month = factor(Month, levels = month.name)) %>%
  group_by(MMWR_Year, Month) %>%
  summarise(Total_Incidents = n(), .groups = "drop")

# 2. Main pipeline
month_counts <- ncan_track %>%
  filter(PD_jail == 0, Narcan == "Yes", MMWR_Year != 2015) %>%
  mutate(Month = factor(Month, levels = month.name)) %>%
  group_by(MMWR_Year, Month) %>%
  mutate(Total_Narcan = n()) %>%  # Total Narcan uses for that month
  filter(Bystander == 1) %>%
  group_by(MMWR_Year, Month, Bystander, Total_Narcan) %>%
  summarise(Bystander_Count = n(), .groups = "drop") %>%
  mutate(Percentage_BS = (Bystander_Count / Total_Narcan) * 100) %>%
  left_join(total_incidents, by = c("MMWR_Year", "Month"))


month_counts

month_counts <- month_counts %>%
  mutate(
    EMS_Narcan_Rate = Total_Narcan / Total_Incidents,
    Bystander_Perc = Bystander_Count / Total_Incidents)

month_counts

test <- lm(EMS_Narcan_Rate ~ Bystander_Perc, data = month_counts)
summary(test)

######
# Split data into two periods
escalation <- Narcan_OT[Narcan_OT$Year <= 2020, ]
decline <- Narcan_OT[Narcan_OT$Year >= 2020, ]

# Escalation period (2016-2020)
escalation_model <- lm(EMS_total ~ Fentaly_percent + Narcan_total + Bystander_total, 
                       data = escalation)
summary(escalation_model)

# Decline period (2020-2025)
decline_model <- lm(EMS_total ~ Fentaly_percent + Narcan_total + Bystander_total, 
                    data = decline)
summary(decline_model)

#####Graphing options for some of the analysis
library(ggplot2)
library(gridExtra)
library(scales)


graph_1 <- ggplot(Narcan_OT, aes(x = Year)) +
  # EMS calls (left axis)
  geom_line(aes(y = EMS_total, color = "EMS Calls"), size = 1.5) +
  geom_point(aes(y = EMS_total, color = "EMS Calls"), size = 3) +
  # Fentanyl % (needs to be scaled for dual axis)
  geom_line(aes(y = Fentaly_percent * 70, color = "Fentanyl %"), size = 1.5) +
  geom_point(aes(y = Fentaly_percent * 70, color = "Fentanyl %"), size = 3) +
  # Add vertical line at 2020 (inflection point)
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray50", size = 0.8) +
  annotate("text", x = 2020.5, y = 6000, label = "Peak Year", 
           hjust = 0, size = 3.5, color = "gray30") +
  # Scales and labels
  scale_x_continuous(breaks = seq(2016, 2025, 1)) +
  scale_y_continuous(
    name = "EMS Calls",
    sec.axis = sec_axis(~./70, name = "Fentanyl Prevalence (%)")
  ) +
  scale_color_manual(values = c("EMS Calls" = "#ef4444", "Fentanyl %" = "#f59e0b")) +
  labs(
    title = "EMS Calls & Fentanyl Prevalence: 2016-2025",
    subtitle = "Both peaked around 2020 and have declined since",
    x = "Year",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(graph_1)


graph_2 <- ggplot(Narcan_OT, aes(x = Year)) +
  # Narcan use (left axis)
  geom_line(aes(y = Narcan_total, color = "Narcan Use"), size = 1.5) +
  geom_point(aes(y = Narcan_total, color = "Narcan Use"), size = 3) +
  # Bystander interventions (scaled for dual axis)
  geom_line(aes(y = Bystander_total * 15, color = "Bystander Interventions"), size = 1.5) +
  geom_point(aes(y = Bystander_total * 15, color = "Bystander Interventions"), size = 3) +
  # Add vertical line at 2020
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray50", size = 0.8) +
  # Scales and labels
  scale_x_continuous(breaks = seq(2016, 2025, 1)) +
  scale_y_continuous(
    name = "Narcan Administrations",
    sec.axis = sec_axis(~./15, name = "Bystander Interventions")
  ) +
  scale_color_manual(values = c("Narcan Use" = "#3b82f6", "Bystander Interventions" = "#10b981")) +
  labs(
    title = "Intervention Efforts: 2016-2025",
    subtitle = "Bystander interventions scaled 14-fold during crisis escalation",
    x = "Year",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(graph_2)


# Normalize all variables to 0-100 scale for comparison
Narcan_OT$EMS_norm <- (Narcan_OT$EMS_total - min(Narcan_OT$EMS_total)) / 
  (max(Narcan_OT$EMS_total) - min(Narcan_OT$EMS_total)) * 100
Narcan_OT$Fentanyl_norm <- (Narcan_OT$Fentaly_percent - min(Narcan_OT$Fentaly_percent)) / 
  (max(Narcan_OT$Fentaly_percent) - min(Narcan_OT$Fentaly_percent)) * 100
Narcan_OT$Narcan_norm <- (Narcan_OT$Narcan_total - min(Narcan_OT$Narcan_total)) / 
  (max(Narcan_OT$Narcan_total) - min(Narcan_OT$Narcan_total)) * 100
Narcan_OT$Bystander_norm <- (Narcan_OT$Bystander_total - min(Narcan_OT$Bystander_total)) / 
  (max(Narcan_OT$Bystander_total) - min(Narcan_OT$Bystander_total)) * 100

graph_3 <- ggplot(Narcan_OT, aes(x = Year)) +
  geom_line(aes(y = EMS_norm, color = "EMS Calls"), size = 1.2) +
  geom_line(aes(y = Fentanyl_norm, color = "Fentanyl %"), size = 1.2) +
  geom_line(aes(y = Narcan_norm, color = "Narcan Use"), size = 1.2) +
  geom_line(aes(y = Bystander_norm, color = "Bystander"), size = 1.2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray50", size = 0.8) +
  scale_x_continuous(breaks = seq(2016, 2025, 1)) +
  scale_color_manual(values = c(
    "EMS Calls" = "#ef4444",
    "Fentanyl %" = "#f59e0b", 
    "Narcan Use" = "#3b82f6",
    "Bystander" = "#10b981"
  )) +
  labs(
    title = "All Variables Normalized (0-100 Scale)",
    subtitle = "Showing relative changes across all measures",
    x = "Year",
    y = "Normalized Value (0-100)",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(graph_3)

graph_4 <- ggplot(Narcan_OT, aes(x = Fentaly_percent, y = EMS_total)) +
  geom_point(size = 4, color = "#8b5cf6") +
  geom_text(aes(label = Year), vjust = -1, size = 3.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#6366f1", linetype = "dashed", alpha = 0.2) +
  labs(
    title = "EMS Calls vs. Fentanyl Prevalence",
    subtitle = "Strong correlation during escalation, breaks down after 2023",
    x = "Fentanyl Prevalence (%)",
    y = "EMS Calls"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(graph_4)

graph_5 <- ggplot(Narcan_OT, aes(x = Bystander_total, y = EMS_total)) +
  geom_point(size = 4, color = "#10b981") +
  geom_text(aes(label = Year), vjust = -1, size = 3.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#059669", linetype = "dashed", alpha = 0.2) +
  labs(
    title = "EMS Calls vs. Bystander Interventions",
    subtitle = "Initial correlation; post-2020 bystander capacity remains high as EMS declines",
    x = "Bystander Interventions",
    y = "EMS Calls"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(graph_5)


# Calculate period statistics
escalation_stats <- data.frame(
  Period = rep("2016-2020\nEscalation", 4),
  Variable = c("EMS Calls", "Fentanyl %", "Narcan Use", "Bystander"),
  Change = c(
    ((5899 - 3074) / 3074) * 100,  # EMS
    ((77.0 - 21.0) / 21.0) * 100,  # Fentanyl
    ((3531 - 1568) / 1568) * 100,  # Narcan
    ((188 - 13) / 13) * 100        # Bystander
  )
)

decline_stats <- data.frame(
  Period = rep("2020-2025\nDecline", 4),
  Variable = c("EMS Calls", "Fentanyl %", "Narcan Use", "Bystander"),
  Change = c(
    ((3869 - 5899) / 5899) * 100,  # EMS
    ((65.3 - 77.0) / 77.0) * 100,  # Fentanyl
    ((1776 - 3531) / 3531) * 100,  # Narcan
    ((151 - 188) / 188) * 100      # Bystander
  )
)

period_stats <- rbind(escalation_stats, decline_stats)

graph_6 <- ggplot(period_stats, aes(x = Variable, y = Change, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  geom_text(aes(label = paste0(round(Change, 1), "%")), 
            position = position_dodge(width = 0.7), 
            vjust = ifelse(period_stats$Change > 0, -0.5, 1.5),
            size = 3.5) +
  scale_fill_manual(values = c("2016-2020\nEscalation" = "#ef4444", 
                               "2020-2025\nDecline" = "#10b981")) +
  labs(
    title = "Percent Change by Period",
    subtitle = "Escalation (2016-2020) vs. Decline (2020-2025)",
    x = "",
    y = "Percent Change (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

print(graph_6)

# ============================================================================
# SAVE ALL PLOTS TO PDF
# ============================================================================

pdf("Overdose_Crisis_Analysis.pdf", width = 11, height = 8.5)
print(graph_1)
print(graph_2)
print(graph_3)
print(graph_4)
print(graph_5)
print(graph_6)
dev.off()

cat("All plots saved to 'Overdose_Crisis_Analysis.pdf'\n")