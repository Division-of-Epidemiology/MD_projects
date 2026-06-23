##Looking for repeat addresses for mapping

repeats <- ncan %>%
  select(Incident_Date, Address, Zip, Age, Gender, MMWR_Year)

repeats <- repeats %>%
  mutate(Incident_Date = mdy(Incident_Date))
         

class(repeats$Incident_Date)


repeat_add <- repeats %>%
  group_by(Address) %>%
  summarise(
    total_calls = n(),
    first_call=min(Incident_Date, na.rm = TRUE),
    last_call=max(Incident_Date, na.rm = TRUE),
    Zip = first(Zip),
    median_age = median(Age, na.rm=TRUE),
    .groups="drop"
  ) %>%
  filter(total_calls > 1) %>%
  arrange(desc(total_calls))


write.csv(repeat_add, "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Documents/CVS_Files/Narcan/Narcan/repeat_add.csv", row.names = FALSE)


repeat_add1620 <- repeats %>%
  filter(MMWR_Year >= 2016 & MMWR_Year <= 2020) %>%
  group_by(Address) %>%
  summarise(
    total_calls = n(),
    first_call=min(Incident_Date, na.rm = TRUE),
    last_call=max(Incident_Date, na.rm = TRUE),
    Zip = first(Zip),
    median_age = median(Age, na.rm=TRUE),
    .groups="drop"
  ) %>%
  filter(total_calls > 1) %>%
  arrange(desc(total_calls))


repeat_add2125 <- repeats %>%
  filter(MMWR_Year >= 2021 & MMWR_Year <= 2025) %>%
  group_by(Address) %>%
  summarise(
    total_calls = n(),
    first_call=min(Incident_Date, na.rm = TRUE),
    last_call=max(Incident_Date, na.rm = TRUE),
    Zip = first(Zip),
    median_age = median(Age, na.rm=TRUE),
    .groups="drop"
  ) %>%
  filter(total_calls > 1) %>%
  arrange(desc(total_calls))

write.csv(repeat_add1620, "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Documents/CVS_Files/Narcan/Narcan/repeat_add1620.csv", row.names = FALSE)
write.csv(repeat_add2125, "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Documents/CVS_Files/Narcan/Narcan/repeat_add2125.csv", row.names = FALSE)

##Looking at the total calls broken down by period like before but now looking at percent change from one chunk to the next
repeats <- repeats %>%
  mutate(
    Period = case_when(
      MMWR_Year >= 2016 & MMWR_Year <= 2020 ~ "2016-2020",
      MMWR_Year >= 2021 & MMWR_Year <= 2025 ~ "2021-2025",
      TRUE ~ NA_character_
    )
  )


address_counts <- repeats %>%
  filter(!is.na(Period)) %>%
  group_by(Address, Period) %>%
  summarise(total_calls=n(),
            Zip = first(Zip),
            .groups="drop")

add_compare <- address_counts %>%
  pivot_wider(
    names_from = Period,
    values_from = total_calls,
    values_fill = 0
  ) %>%
  mutate(
    change = `2021-2025` - `2016-2020`,
    perct_change = ifelse(`2016-2020`==0, NA, round((`2021-2025`-`2016-2020`)/`2016-2020`*100))
  ) %>%
  arrange(desc(change))


write.csv(add_compare, "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Documents/CVS_Files/Narcan/Narcan/address_comparison.csv", row.names = FALSE)

##Looking at just 2025
counts_25 <- repeats %>%
  filter(MMWR_Year == 2025) %>%
  group_by(Address) %>%
  mutate(total_calls=n()) %>%
  filter(total_calls > 1) %>%
  select(-Age, -Gender, -Period, -Incident_Date) %>%
  distinct(Address, .keep_all = TRUE) %>%
  arrange(desc(total_calls)) 

write.csv(counts_25, "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Documents/CVS_Files/Narcan/Narcan/address_comparison_2025.csv", row.names = FALSE)


head(counts_25, 10)

