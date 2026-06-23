

me_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ME_25.csv")


me_data <- me_data %>%
  mutate(
    has_opioids = grepl("Synthetic Opioid|Natural/Semi-Synthetic Opioid|Opioid|Fentanyl",
                        substance_list, ignore.case = TRUE),
    has_stims = grepl("Cocaine|Methamphetamine|Psychostimulant",
                      substance_list, ignore.case = TRUE),
    has_alcohol = grepl("Alcohol",
                        substance_list, ignore.case = TRUE),
    has_benzos = grepl("Benzodiazepine",
                       substance_ct, ignore.case = TRUE),
    
    substance_bucket = case_when(
      has_opioids & has_stims ~ "Mixed",
      has_opioids & !has_stims ~ "Opioid Dominant",
      has_stims & !has_opioids ~ "Stimulant Dominant",
      has_benzos & !has_opioids & !has_stims ~ "Benzo Dominant",
      TRUE ~ "Other"),
      
    alcohol_co_involved = case_when(
      has_alcohol ~ "Alochol Co-Involved",
      TRUE ~ "Other")
    )

cleaned_me <- me_data %>%
  select(Zip, Injury_Location, IL_City, IL_State, substance_bucket,alcohol_co_involved)


write.csv(cleaned_me, "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/substance_me.csv")
