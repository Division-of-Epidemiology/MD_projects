##CLeaning for heat related ED visits 
library(dplyr)
library(tidyr)
library(ggplot2)

HR_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/Heat_related_2226.csv")



triage_labels <- c("1 - Resuscitation", "2 - Emergent", "3 - Urgent", "4 - Less Urgent", "5 - Non-Urgent")

HR_clean <- HR_data %>%
  select(PIN, Date, Zipcode, Sex, Age, c_race, Chief_Complaint_Combo, CCDDCategory_flat, Initial_Acuity_Combo) %>%
  mutate(Initial_Acuity_Combo = as.integer(sub("^(\\d+).*", "\\1", Initial_Acuity_Combo)),
         Initial_Acuity_Combo = factor(recode(Initial_Acuity_Combo,
                                              `1` = "1 - Resuscitation",
                                              `2` = "2 - Emergent",
                                              `3` = "3 - Urgent",
                                              `4` = "4 - Less Urgent",
                                              `5` = "5 - Non-Urgent"
         ), levels=triage_labels, ordered=TRUE),
         Housing_status = case_when(
           grepl("Persons Experiencing Homelessness", CCDDCategory_flat, ignore.case = TRUE) ~ "Unhoused",
           TRUE ~ "Other"),
           Age_group = as.numeric(gsub("[^0-9]", "", Age)),
           Age_group = cut(
             Age_group,
             breaks = c(-Inf, 18, 24, 34, 44, 54, 64, Inf),
             labels = c("0-18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
             right = TRUE,
             ordered_result = TRUE
           ),
           Year = as.numeric(format(as.Date(Date, format = "%m/%d/%Y"), "%Y"))
         ) 


##now we graph
##Percentage of total
plot1 <- HR_clean %>%
  count(Initial_Acuity_Combo) %>%
  mutate(pct=scales::percent(n/sum(n))) %>%
  ggplot(aes(x=Initial_Acuity_Combo, y=n, fill=Initial_Acuity_Combo)) +
  geom_col() +
  geom_text(aes(label = pct), vjust=-0.5) +
  labs(
    title="Triage Level Distribution",
    x="Triage Level",
    y="Percentage of Total"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5))

##looking at housing status

plot2 <- HR_clean %>%
  count(Housing_status) %>%
  mutate(pct=scales::percent(n/sum(n))) %>%
  ggplot(aes(x=Housing_status, y=n, fill=Housing_status)) +
  geom_col() +
  geom_text(aes(label = pct), vjust=-0.5) +
  labs(
    title="Housing Status of ED Admission",
    x="Housing Status",
    y="Total Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5))



