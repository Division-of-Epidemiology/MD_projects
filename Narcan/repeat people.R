##Looking at repeat names in 2025 in hopes to look years back to find first instance than mayb elook at ME

EMS_2326 <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Documents/CVS_Files/Narcan/Narcan/EMS_2326.csv")



EMS_2326$Incident_Date <- as.Date(EMS_2326$Incident_Date)



repeat_people <- EMS_2326 %>%
  group_by(First_Name, Last_Name) %>%
  summarise(
    n = n(),
    first_seen = min(Incident_Date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n > 1) %>%
  arrange(desc(n))