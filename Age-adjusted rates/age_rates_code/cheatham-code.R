##Setting up the packages for calculating Age-adjusted rates for OD's 2020-2023 for Cheatham Co

library(knitr)
library(epitools)
library(ggplot2)

##setting up a dataframe for each population for comparison

#Labels for the age groups

age_groups <- c("0-4","5-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")
population_23 <- c(2232,	2035,	2941,	2567,	2202,	5284,	5200,	5971,	6044, 3944,	1617,	502)
population_22 <- c(2232,	2035,	2941,	2567,	2202,	5284,	5200,	5971,	6044, 3944,	1617,	502)
population_21 <- c(2232,	2035,	2941,	2567,	2202,	5284,	5200,	5971,	6044, 3944,	1617,	502)
population_20 <- c(2232,	2035,	2941,	2567,	2202,	5284,	5200,	5971,	6044, 3944,	1617,	502)

#Setting the deaths in each population 
deaths_23 <- c(0,	0,	0,	1,	0,	4,	7,	1,	1,	3,	0,	0)
deaths_22 <- c(0,	0,	0,	1,	0,	5,	7,	9,	6,	0,	0,	0)
deaths_21 <- c(0,	0,	0,	0,	1,	5,	6,	8,	0,	0,	0,	0)
deaths_20 <- c(0,	0, 0,	3,	0,	9,	7,	6,	3,	1,	0,	0)

##Standard population
std_pop <- c(0.06913565,
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
df_23 <- data.frame(age=age_groups, deaths = deaths_23, pop=population_23, group = '2023')
df_22 <- data.frame(age=age_groups, deaths=deaths_22, pop=population_22, group='2022')
df_21 <- data.frame(age=age_groups, deaths=deaths_21, pop=population_21, group='2021')
df_20 <- data.frame(age=age_groups, deaths=deaths_20, pop=population_20, group='2020')


df_all <- rbind(df_23, df_22, df_21, df_20)
df_pop <- data.frame(age=age_groups, pop=std_pop)



##Running epitools and storing them in a table 
pop_23_adjust <- ageadjust.direct(deaths_23, population_23, rate=NULL, 
                                  std_pop, conf.level = 0.95)
Age_Adjusted23 <- 10^5*pop_23_adjust


pop_22_adjust <- ageadjust.direct(deaths_22, population_22, rate=NULL, 
                                  std_pop, conf.level = 0.95)
Age_Adjusted22 <- 10^5*pop_22_adjust


pop_21_adjust <- ageadjust.direct(deaths_21, population_21, rate=NULL, 
                                  std_pop, conf.level = 0.95)
Age_Adjusted21 <- 10^5*pop_21_adjust


pop_20_adjust <- ageadjust.direct(deaths_20, population_20, rate=NULL, 
                                  std_pop, conf.level = 0.95)
Age_Adjusted20 <- 10^5*pop_20_adjust


##Creating a dataframe with all the results to graph the differences
Results <- data.frame(rbind(Age_Adjusted20, Age_Adjusted21, Age_Adjusted22, Age_Adjusted23), 
                      Year = c(2020, 2021, 2022, 2023))
colnames(Results) <- c('Crude_Rate', 'Adj_Rate', 'Low_CI', 'High_CI', 'Year')

Results

##Plotting the results

ggplot(Results, aes(x = Year)) +
  geom_line(aes(y = Crude_Rate, color = "Crude Rate"), linewidth = 1.2) +
  geom_line(aes(y = Adj_Rate, color = "Adjusted Rate"), linewidth = 1.2) +
  geom_errorbar(aes(ymin = Low_CI, ymax = High_CI), width = 0.2, color = "black") +
  labs(title = "Crude and Adjusted Rates with Confidence Intervals",
       x = "Year",
       y = "Rate",
       color = "Rate Type") +
  theme_minimal() +
  scale_color_manual(values = c("Crude Rate" = "blue", "Adjusted Rate" = "red")) +
  theme(legend.position = "bottom")

