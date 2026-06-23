getwd()
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)



narcan <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Documents/CVS_Files/Narcan/Narcan/EMS_Tableau_Weekly.csv")

narcan <- narcan %>% 
  mutate(Bystander = ifelse(str_detect(Incident_Narrative, fixed("Narcan")) |
                              str_detect(Incident_Narrative, regex("narc\\s*an|narcan|naloxone|naloxone\\s*hydrochloride|naloxon|nalozone", ignore_case=TRUE)),
                            1, 0))

narcan_track <- narcan %>% select(Incident_Date, Age, Gender, Race, ZIP, Med_Admin, Dosage, Bystander) %>%
  mutate(Med_Admin= if_else(Med_Admin == "", "None", Med_Admin),
         Dosage = if_else(Dosage == "", "None", Dosage),
         Narcan = if_else(grepl("narcan", Med_Admin, ignore.case=TRUE)| Bystander ==1, "Yes", "No"))

head(narcan_track)

incident_counts <- narcan_track %>%
  group_by(Narcan) %>%
  summarise(Count = n())

# Create the bar plot
ggplot(incident_counts, aes(x = Narcan, y = Count, fill = Narcan)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Total Count of Incidents by Narcan Administration",
       x = "Narcan Administered",
       y = "Total Count of Incidents") +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red"))

##Looking at all the unique options in the Med_Adim column
admin_type <- narcan %>% 
  group_by(Med_Admin) %>% 
  summarise(count=n())
print(admin_type)

##Looking at the dosages for Narcan alone 
dosage_type <- narcan %>%
  filter(trimws(Med_Admin) == "Narcan (Naloxone)") %>%
  group_by(Dosage) %>%
  summarise(count = n())
  
print(dosage_type)

##Trying to pull out Bystander Narcan use from EMS data
bystander_narcan <- narcan %>%
  filter(Med_Admin != "Narcan (Naloxone)") %>%
  group_by(Bystander) %>%
  summarise(count = n())

print(bystander_narcan)


EMS_narcan <- narcan %>%
  filter(Med_Admin == "Narcan (Naloxone)") %>%
  group_by(Bystander) %>%
  summarise(EMS_count = n())
print(EMS_narcan)

##Creating a dataset for all Narcan counts either by EMS or Bystander or both
# Filter for Narcan in the med-admin column
narcan_filtered <- narcan %>%
  filter(str_detect(Med_Admin, fixed("Narcan", ignore_case = TRUE)))

# Filter for bystander_narcan equal to 1 and no narcan found in Med_admin
bystander_filtered <- narcan %>%
  filter(Bystander == 1 & !str_detect(Med_Admin, fixed("Narcan", ignore_case=TRUE)))

# Combine the two datasets
combined_filtered_data <- bind_rows(narcan_filtered, bystander_filtered) %>%
  distinct() 

combined_filtered_data <- combined_filtered_data %>% 
  select(Incident_Date, Age, Gender, Race, ZIP, Med_Admin, Dosage, Bystander)
print(combined_filtered_data)

##Remove any blanks from the demographic columns for analysis
cleaned_data <- combined_filtered_data %>%
  filter(!is.na(Race) & Race != "",
         !is.na(Gender) & Gender != "",
         !is.na(Age) & Age != "") %>%
  filter(Gender !="Unknown (Unable to Determine)", Race != "Unknown") %>%
  mutate(Race = case_when(
    grepl("^Caucasian", Race) ~ "White",
    grepl("^African American", Race) ~ "African American",
    grepl("^Asian", Race) ~ "Asian",
    grepl("^Hispanic", Race) ~ "Hispanic or Latino",
    TRUE ~ "Other" )) %>%
  mutate(age_group = cut(
    as.numeric(Age),
    breaks = c(-Inf, 0, 17, 24, 34, 44, 54, 64, Inf),
    labels = c("0-","0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    right = TRUE
  ))

cleaned_data <- cleaned_data %>%
  mutate(Year = year(mdy(Incident_Date)))

##Analyizing the total Narcan data set for 2023-YTD2024

##Race and gender
R_G_distribution <- cleaned_data %>%
  group_by(Race, Gender) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  mutate(percentage = count/sum(count)*100)

print(R_G_distribution)

demogrpahic <- table(cleaned_data$Gender, cleaned_data$Race, cleaned_data$Year)
demo_df <- as.data.frame(demogrpahic)

colnames(demo_df) <- c("Gender", "Race", "Year", "Count")

ggplot(demo_df, aes(x = Race, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Gender and Race Distribution by Year",
       x = "Race",
       y = "Count") +
  facet_wrap(~ Year)


##Age group
AG_distribution <- cleaned_data %>%
  group_by(age_group, Year) %>% 
  summarise(count=n(),
            proportion = n()/nrow(cleaned_data)*100, .groups='drop')

ggplot(AG_distribution, aes(x = age_group, y = count, fill = age_group)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Age Group Distribution", x = "Age Group", y = "Count") +
  facet_wrap(~ Year)



##Looking by zip code 

zip_dist <- table(cleaned_data$ZIP, cleaned_data$Year)
zip_df <- as.data.frame(zip_dist)
colnames(zip_df) <- c("Zip_Code", "Year", "Count")




##Plotting by Year since there are alot of zip codes
unique_years <- unique(zip_df$Year)

for (current_year in unique_years) {
  year_data <- subset(zip_df, Year == current_year)
  
  top_zips <- year_data %>% arrange(desc(Count)) %>% head(20)
  
  print(paste("Year:", current_year, "- Top Zip Codes:", nrow(top_zips)))
  
  if (nrow(top_zips) > 0) {
  p <- ggplot(top_zips, aes(x=Zip_Code, y=Count, fill=Zip_Code)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    labs(title=paste("Zip Code Distribution for Year:", current_year), x="Zip Code", y="Count") +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  print(p)
  } else {
  message(paste("No data available for year: ", current_year))
}
}


narcan_track <- narcan_track %>%
  mutate(ZIP = as.character(ZIP))

##Look at levels of activity by zip code compared with narcan admin
incident_counts_zip <- narcan_track %>%
  group_by(ZIP, Narcan) %>%
  summarise(Count = n(), .groups = 'drop')

# Identify the top 15 ZIP codes based on total incident counts
top_zip_codes <- incident_counts_zip %>%
  group_by(ZIP) %>%
  summarise(Total_Count = sum(Count)) %>%
  top_n(15, Total_Count) %>%
  pull(ZIP)

# Filter the original data for only the top 15 ZIP codes
top_incident_counts_zip <- incident_counts_zip %>%
  filter(ZIP %in% top_zip_codes)

# Create the bar plot
ggplot(top_incident_counts_zip, aes(x = ZIP, y = Count, fill = Narcan)) +
  geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
  theme_minimal() +
  labs(title = "Total Count of Incidents by Top 15 ZIP Codes and Narcan Administration",
       x = "ZIP Code",
       y = "Total Count of Incidents") +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red")) +  # Custom colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

