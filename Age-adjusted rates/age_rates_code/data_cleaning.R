##Calculating each age group and total number of deaths by year
library(readxl)
library(dplyr)

base_path <- "O:/Drug Overdose Surveillance/Medical Examiner/DataFrames.py"
years <- 17:25

me_data <- lapply(years, function(yr){
  read_excel(file.path(base_path, paste0("Final_20", yr, ".xlsx")))
})

names(me_data) <- paste0("me_", years)

##counting totals per age group
age_breaks <- c(0,5,10,15,20,25,35,45,55,65,75,85, Inf)
age_labels <- c("0-4","5-9","10-14","15-19","20-24","25-34","35-44","45-54","55-64","65-74","75-84","85+")


me_data <- lapply(me_data, function(df){
  df %>%
    mutate(age_break = cut(age_num, 
                           breaks = age_breaks,
                           labels = age_labels,
                           right=FALSE))
           })

age_counts <- lapply(me_data, function(df) {
  df_res <- df %>% filter(resident == "Yes")
  
  list(
    total  = as.data.frame(table(df_res$age_break)),
    male   = as.data.frame(table(df_res$age_break[df_res$Gender == "Male"])),
    female = as.data.frame(table(df_res$age_break[df_res$Gender == "Female"])),
    white  = as.data.frame(table(df_res$age_break[df_res$Race == "White"])),
    black  = as.data.frame(table(df_res$age_break[df_res$Race == "Black"]))
  )
})

names(age_counts) <- paste0("deaths_", 17:25)
list2env(age_counts, envir = .GlobalEnv)


#######
demo_counts <- lapply(me_data, function(df){
  df_res <- df %>% filter(resident == "Yes")
  
  list(
    total = nrow(df_res),
    male = sum(df_res$Gender == "Male", na.rm=TRUE),
    female = sum(df_res$Gender == "Female", na.rm=TRUE),
    white = sum(df_res$Race == "White", na.rm=TRUE),
    black = sum(df_res$Race == "Black", na.rm=TRUE)
  )
})

names(demo_counts) <- paste0("demos_", 17:25)
list2env(demo_counts, envir = .GlobalEnv)
