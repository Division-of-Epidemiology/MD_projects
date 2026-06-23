##Loading in and cleaning the EMS data with the new columns added in
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readxl)


file_paths <- c("C:/Users/mdickson/OneDrive - Metro Nashville Government/Documents/CVS_Files/Narcan/Narcan/2025 Updated Columns/Fire_2020.xlsx",
                "C:/Users/mdickson/OneDrive - Metro Nashville Government/Documents/CVS_Files/Narcan/Narcan/2025 Updated Columns/Fire_2021.xlsx",
                "C:/Users/mdickson/OneDrive - Metro Nashville Government/Documents/CVS_Files/Narcan/Narcan/2025 Updated Columns/Fire_2022.xlsx",
                "C:/Users/mdickson/OneDrive - Metro Nashville Government/Documents/CVS_Files/Narcan/Narcan/2025 Updated Columns/Fire_2023.xlsx",
                "C:/Users/mdickson/OneDrive - Metro Nashville Government/Documents/CVS_Files/Narcan/Narcan/2025 Updated Columns/Fire_2024.xlsx")
data_list <- lapply(file_paths, read_excel)

combined_data <- bind_rows(data_list)

new_ncan <- combined_data %>%
  arrange(Year)

ncan_dedup <- new_ncan %>%
  distinct(`Incident Number`, .keep_all=TRUE)

ncan_dedup$`Medication Administered Prior To EMS Unit Care (eMedications.02)`[is.na(ncan_dedup$`Medication Administered Prior To EMS Unit Care (eMedications.02)`)] <- "No"
ncan_dedup$`Medication Role Of Person Administering Medication (eMedications.10)`[is.na(ncan_dedup$`Medication Role Of Person Administering Medication (eMedications.10)`)] <- "No"



ncan_dedup$Year <- year(ncan_dedup$'Incident Date')


PTA_counts <- ncan_dedup %>%
  group_by(Year, `Medication Administered Prior To EMS Unit Care (eMedications.02)`) %>%
  summarise(Count = n())

PTA_counts

Layperson_counts <- ncan_dedup %>%
  group_by(Year, `Medication Role Of Person Administering Medication (eMedications.10)`) %>%
  filter(`Medication Role Of Person Administering Medication (eMedications.10)` == "Patient/Lay Person") %>%
  summarise(Count=n())

Layperson_counts  
  
  
  





