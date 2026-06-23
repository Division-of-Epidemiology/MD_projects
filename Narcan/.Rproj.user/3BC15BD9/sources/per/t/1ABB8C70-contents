##Looking at percentage of Bystander NArcan usage in EMS data 2018-2024
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)




ncan <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Documents/CVS_Files/Narcan/Narcan/EMS_Tableau_Quarter.csv")

names(ncan)

ncan <- ncan %>% 
  mutate(Bystander = ifelse(
    # Check if a bystander administered Narcan (corrected regex)
    str_detect(ncan$Incident_Narrative, regex(
      "(layperson|by[ ]?stander|friend|family|wife|mother|father|staff|spouse)\\b.*?(have|given|gave|administered|used|provided)\\b.*?\\b(narcan)\\b|\\b(narcan)\\b.*?(have|gave|administered|used|provided|given)\\b.*?(staff|layperson|by[ ]?stander|friend|wife|family|mother|father|spouse)",
      ignore_case = TRUE)) |
      str_detect(ncan$Incident_Narrative, regex(
        "(prior to arrival|before we got there|prior to EMS arrival|uoa)\\b.*?(have|given|gave|administered|used|provided)\\b.*?\\b(narcan)\\b|\\b(narcan)\\b.*?(have|gave|administered|used|provided|given)\\b.*(prior to arrival|before we got there|prior to EMS arrival|uoa)", 
        ignore_case = TRUE)) &
      
      # Exclude cases where EMS or Fire administered Narcan (correction to exclude medical personnel)
      !str_detect(ncan$Incident_Narrative, regex(
        "(EMS|fire|paramedic|ambulance|medic|firefighter|first responders|Pt).*narcan",
        ignore_case = TRUE)),
    1, 0),
    
    # PD_jail flag: Narcan + Jail-related
    PD_jail = ifelse(
      str_detect(ncan$Incident_Narrative, regex(
        "(given|administered|used|gave).*narcan|narcan.*(given|administered|used|gave)", 
        ignore_case = TRUE)) &
        str_detect(ncan$Incident_Narrative, regex(
          "\\b(jail|prison|corrections|officer|PD|MNPD|NPD|jail staff)\\b", 
          ignore_case = TRUE)),
      1, 0),
    
    # FIRE_EMS flag: Narcan + Fire or EMS-related
    FIRE_EMS = ifelse(
      str_detect(ncan$Incident_Narrative, regex(
        "(given|administered|used|gave).*narcan|narcan.*(given|administered|used|gave)", 
        ignore_case = TRUE)) &
        str_detect(ncan$Incident_Narrative, regex(
          "\\b(fire|FIRE|Engine|crew|EMS|first responders)\\b", 
          ignore_case = TRUE)),
      1, 0))

FIRE_EMS = ifelse(
  str_detect(ncan$Incident_Narrative, regex(
    "(given|administered|used|gave).*narcan|narcan.*(given|administered|used|gave)", 
    ignore_case = TRUE)) &
    str_detect(ncan$Incident_Narrative, regex(
      "\\b(fire|FIRE|Engine|crew|EMS|first responders)\\b", 
      ignore_case = TRUE)),
  1, 0)


ncan$Incident_Date <- mdy(ncan$Incident_Date)
ncan$Year <- year(ncan$Incident_Date)

ncan_track <- ncan %>% select(Incident_Date, Incident_Narrative, Age, Gender, Race,Address, ZIP, ZIP1, Med_Admin, Dosage, Bystander, Dest_Pt_Disp, Disposition_Patient_Disposition, Medication_Administered, MMWR_Year, PD_jail, Address,Incident_Postal_Code) %>%
  mutate(across(c(Incident_Narrative, Medication_Administered, Med_Admin, Dosage),
                ~ iconv(as.character(.), to="UTF-8", sub="")),
         Med_Admin= if_else(is.na(Med_Admin) | Med_Admin == "", "None", Med_Admin),
         Medication_Administered = if_else(is.na(Medication_Administered) | Medication_Administered == "", "None", Medication_Administered),
         Dosage = if_else(is.na(Dosage) | Dosage == "", "None", Dosage),
         Narcan = if_else(
           grepl("narcan", Med_Admin, ignore.case = TRUE) | 
             grepl("narcan", Medication_Administered, ignore.case = TRUE) | 
             grepl("narcan", Incident_Narrative, ignore.case=TRUE) |
             Bystander == 1, 
           "Yes", 
           "No")) 