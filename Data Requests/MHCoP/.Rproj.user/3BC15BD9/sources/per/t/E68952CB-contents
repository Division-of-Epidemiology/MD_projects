##Script for cleaning data from ESSENCE for MHCoP for their sobering center
##Looking at specific ICD-10 codes involving intoxication

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

getwd()
data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/alcohol_MHCOP.csv")

class(data$Date)
data$Date <- mdy(data$Date)

data$Year <- year(data$Date)
data$Month <- month(data$Date, label=TRUE, abbr=FALSE)
data$DOW <- wday(data$Date, label = TRUE, abbr=FALSE)


data_cleaned <- data %>%
  select(PIN, Date, Facility.Name, Zipcode, Sex, Age, Year, Month, DOW) %>%
  distinct(PIN, .keep_all = TRUE) %>%
  mutate(YearMonth = floor_date(Date, "month"))

monthly_counts <- data_cleaned %>%
  group_by(YearMonth) %>%
  summarise(count = n())

ggplot(monthly_counts, aes(YearMonth, count)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) +
  scale_x_date(date_labels = "%b %Y", breaks = monthly_counts$YearMonth) +
  labs(
    title = "Monthly Unique PIN Counts",
    subtitle = "July 2025 – July 2026",
    x = "Month",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40")
  )

##Looking at burden by hosptial
locations <- data_cleaned %>%
  group_by(Facility.Name) %>%
  summarise(count = n())

ggplot(locations, aes(Facility.Name, count)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) +
  labs(
    title = "Total Incidents by Facility Name",
    subtitle = "July 2025 – July 2026",
    x = "Facility Name",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40")
  )

##Break it down by the day of the week 
DOW <- data_cleaned %>%
  group_by(DOW) %>%
  summarise(count=n())

ggplot(DOW, aes(DOW, count)) +
  geom_col(fill = "#2C7FB8") +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) +
  labs(
    title = "Total Incidents by Day of Week",
    subtitle = "July 2025 – July 2026",
    x = "Day of Week",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40")
  )
