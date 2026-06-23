##Cleaning the crime data and compiling into non-violent and violent crimes by zip code

library(dplyr)
library(tidyr)
library(stringr)


##Load in data
getwd()

crime_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/crime_data.csv")

non_violent_labels <- c("Auto Theft", "Larceny", "Commercial Burglary", "Residential Burglary")
violent_labels <- c("Homicide", "Street Robbery", "Commercial Robbery", "Aggravated Assault")




crime_zip <- crime_data %>%
  group_by(Zip, lbl) %>%
  summarize(Count = n(), .groupd = "drop") %>%
  mutate(Category = case_when(
    lbl %in% non_violent_labels ~ "Non-violent",
    lbl %in% violent_labels ~ "Violent", 
    TRUE ~ "Other"
  )) %>%
  group_by(Zip, Category) %>%
  summarize(Total=sum(Count), .groups="drop") %>%
  pivot_wider(names_from = Category, values_from = Total, values_fill = 0)


write.csv(crime_zip, "C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/crime_zip.csv", row.names = FALSE)


