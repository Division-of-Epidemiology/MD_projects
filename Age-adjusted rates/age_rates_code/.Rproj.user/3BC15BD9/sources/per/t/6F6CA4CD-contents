##Setting up the packages for calculating Age-adjusted rates for OD's 2020-2023 Davidson county

library(knitr)
library(epitools)
library(ggplot2)
library(dplyr)

##setting up a dataframe for each population for comparison

#Labels for the age groups

age_groups <- c("0-4","5-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")
population_25 <- c(45667,	38317,	38099,	41289,	51609,	143540,	105969,	78209,	74668,	59296,	26628,	9043)
population_24 <- c(45667,	38317,	38099,	41289,	51609,	143540,	105969,	78209,	74668,	59296,	26628,	9043)
population_23 <- c(45667,	38317,	38099,	41289,	51609,	143540,	105969,	78209,	74668,	59296,	26628,	9043)
population_22 <- c(44828,	38188,	36115,	39392,	52303,	143714,	102970,	78549,	76849,	58627,	26747,	9865)
population_21 <- c(44557,	39323,	36648,	40250,	49306,	141718,	101557,	78972,	78864,	59654,	24241,	8863)
population_20 <- c(45360,	38438,	36554,	38448,	49411,	143713,	96816,	77472,	78352,	55347,	24348,	9911)
population_19 <- c(46636, 41531,  36517,  38458,  51154,  132001, 92902,  83701,  77967,  45714,  22559,  9182)
population_18 <- c(46636, 41531,  36517,  38458,  51154,  132001, 92902,  83701,  77967,  45714,  22559,  9182)
population_17 <- c(46636, 41531,  36517,  38458,  51154,  132001, 92902,  83701,  77967,  45714,  22559,  9182)

#Setting the deaths in each population 
death_25 <- deaths_25$Freq
death_24 <- deaths_24$Freq
death_23 <- deaths_23$Freq
death_22 <- deaths_22$Freq
death_21 <- deaths_21$Freq
death_20 <- deaths_20$Freq
death_19 <- deaths_19$Freq
death_18 <- deaths_18$Freq
death_17 <- deaths_17$Freq

##Standard population
pop <- c(0.06913565,
             0.072532898,
             0.073031744,
             0.07216878,
             0.06647757,
             0.135573163,
             0.162612786,
             0.134833997,
             0.087247027,
             0.06603698,
             0.044841498,
             0.015507912)


##Now we need to create a dataframe with the vectors created above
years <- 2017:2025
deaths_list <- list(death_17, death_18, death_19, death_20, death_21, death_22, death_23, death_24, death_25)
pop_list <- list(population_17,population_18,population_19,population_20,population_21,population_22,population_23, population_24, population_25)

df_list <- mapply(function(deaths, pop, year) {
  data.frame(age = age_groups, deaths=deaths, pop=pop, group=as.character(year))
  
}, deaths_list, pop_list, years, SIMPLIFY = FALSE)

names(df_list) <- paste0('df_', years)

df_25 <- df_list[['df_2025']]
df_24 <- df_list[['df_2024']]
df_23 <- df_list[['df_2023']]
df_22 <- df_list[['df_2022']]
df_21 <- df_list[['df_2021']]
df_20 <- df_list[['df_2020']]
df_19 <- df_list[['df_2019']]
df_18 <- df_list[['df_2018']]
df_17 <- df_list[['df_2017']]


df_all <- rbind(df_25, df_24, df_23, df_22, df_21, df_20, df_19, df_18, df_18)
df_pop <- data.frame(age=age_groups, pop=std_pop)


##Running epitools and storing them in a table 
Calculate_age_adjusted <- function(deaths, population, std_pop, conf.level=0.95) {
  pop_adjust <- ageadjust.direct(deaths, population, rate=NULL, stdpop=pop, conf.level = conf.level)
  Age_adjusted <- 10^5 * pop_adjust
  return(Age_adjusted)
}

Age_adjusted25 <- Calculate_age_adjusted(death_25, population_25)
Age_adjusted24 <- Calculate_age_adjusted(death_24, population_24)
Age_adjusted23 <- Calculate_age_adjusted(death_23, population_23)
Age_adjusted22 <- Calculate_age_adjusted(death_22, population_22)
Age_adjusted21 <- Calculate_age_adjusted(death_21, population_21)
Age_adjusted20 <- Calculate_age_adjusted(death_20, population_20)
Age_adjusted19 <- Calculate_age_adjusted(death_19, population_19)
Age_adjusted18 <- Calculate_age_adjusted(death_18, population_18)
Age_adjusted17 <- Calculate_age_adjusted(death_17, population_17)



##Creating a dataframe with all the results to graph the differences
Results <- data.frame(rbind(Age_adjusted17,Age_adjusted18, Age_adjusted19, Age_adjusted20, Age_adjusted21, Age_adjusted22, Age_adjusted23, Age_adjusted24, Age_adjusted25), 
                      Year = c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025))
colnames(Results) <- c('Crude_Rate', 'Adj_Rate', 'Low_CI', 'High_CI', 'Year')

Results


## Adding in the rates from CDC for comparison
state_rate <- c(29.3, 42.5, 53.5, 52.9, 49.5)
national_rate <-c(20, 26.3, 30.8, 30.9, 30.1)

sd(national_rate)

Other <- data.frame(cbind(state_rate, national_rate),
                    Year = c(2019, 2020, 2021, 2022, 2023))

results_filtered <- Results %>%
  filter(Year %in% c(2019, 2020, 2021, 2022, 2023))

##Plotting the results
ggplot(results_filtered, aes(x = Year)) +
  geom_line(aes(y = Crude_Rate, color = "DVD Crude Rate"), linewidth = 1.2) +
  geom_line(data=Other, aes(y=state_rate, color="State Rate"), linewidth = 1.2) +
  geom_line(data=Other, aes(y=national_rate, color="National Rate"), linewidth=1.2)+
  labs(title = "Overdose Mortality Rates, 2019-2023",
       x = "Year",
       y = "Rate per 100,000 Persons",
       color = "Rate Type") +
  theme_minimal() +
  scale_color_manual(values = c("DVD Crude Rate" = "red", "State Rate"= "green", "National Rate"="purple")) +
  theme(legend.position = "right")+
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.2, "cm"),   # Increase the size of the legend keys
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", size = 0.5),  # Subtle gridlines
    panel.grid.minor = element_line(color = "gray95", size = 0.25) # Even subtler gridlines
  )

##Breaking down for each age group in 2025

age_rates <- data.frame(age=age_groups, deaths=death_25, pop=population_25)

age_rates <- age_rates %>%
  mutate(Rate = (deaths/pop)*10^5,
         CI_lower = (10^5/pop)*(deaths-1.96*(sqrt(deaths))),
         CI_upper = (10^5/pop)*(deaths+1.96*(sqrt(deaths))))
age_rates

(9/92898)*10^5
(10^5/92898)*(9-1.96*(sqrt(9)))

s

              