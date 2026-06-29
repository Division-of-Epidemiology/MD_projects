library(dplyr)
library(tidyr)
library(ggplot2)

getwd()
suicide_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/suicide_ME_2026.csv")


## Age grouping ##
suicide_data <- suicide_data %>%
  mutate(
    Age_group = as.numeric(gsub("[^0-9]", "", Age)),
    Age_group = cut(
      Age_group,
      breaks = c(-Inf, 18, 24, 34, 44, 54, 64, Inf),
      labels = c("0-18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
      right = TRUE,
      ordered_result = TRUE
    ),
    Year = as.numeric(format(as.Date(DOD, format = "%m/%d/%Y"), "%Y"))
  )
  