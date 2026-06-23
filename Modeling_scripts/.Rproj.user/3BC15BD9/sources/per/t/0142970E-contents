##Attaching EMS counts by zip code to the policymap data.
library(MASS)
library(dplyr)
library(tidyr)
library(stringr)
library(AER)
library(car)
library(ggplot2)
library(ggeffects)
library(gridExtra)
library(ggplot2)


PM_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/zip_policymap.csv")
ems_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/EMS_24.csv")
ME_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ME_2024.csv")
Rops_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ROPS_24.csv")


write.csv(Rops_data,"C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ROPS_24_zip.csv")

##Create a master list of zip codes from policy map to only look at those across datasets
master_zips <- PM_data$GeoID
master_zips

##filter and aggregate per zip code total EMS and ME 
ems_data <- ems_data %>%
  group_by(Zip) %>%
  summarize(Total_ems = n())

ME_data <- ME_data %>% 
  group_by(Zip) %>% 
  summarize(Total_ME =n())


Rops_data <- Rops_data %>%
  group_by(Zip.Code) %>%
  summarize(Total_narcan = n())

##Pull out only matching zip code information from all three datasets then combine into one larger set 
EMS_zip <- ems_data[ems_data$Incident.Postal.Code %in% master_zips, ]
crime_filtered <- crime_zip[crime_zip$Zip %in% master_zips, ]
ME_zip <- ME_data[ME_data$Zip %in% master_zips, ]
Rops_zip <- Rops_data[Rops_data$Zip.Code %in% master_zips, ]


combined_data <- PM_data %>%
  left_join(EMS_zip, by = c("GeoID" = "Incident.Postal.Code")) %>%
  left_join(crime_filtered, by = c("GeoID" = "Zip")) %>%
  left_join(ME_zip, by = c("GeoID" = "Zip")) %>%
  left_join(Rops_zip, by = c("GeoID" = "Zip.Code"))

##Filter out unwanted columns for make it easier to work with
cleaned_data <- combined_data %>%
  select(GeoID, Total_ems, Total_ME, `Non-violent`, Violent, rpopden, median_age, median_income, low.ed, percent_nonwhite, uninsured, Total_narcan)

##look at each variable and ems calls to visualize correlation
ggplot(cleaned_data, aes(low.ed, Total_ems))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE) 

cor(cleaned_data$Total_ems, cleaned_data$low.ed) ###0.622

ggplot(cleaned_data, aes(Violent, Total_ems))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

cor(cleaned_data$Total_ems, cleaned_data$Violent) ###0.904

ggplot(cleaned_data, aes(cleaned_data$`Non-violent`, Total_ems))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

cor(cleaned_data$Total_ems, cleaned_data$`Non-violent`) ###0.849


ggplot(cleaned_data, aes(sqrt(rpopden), sqrt(Total_ems)))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

cor(cleaned_data$Total_ems, cleaned_data$rpopden) ###0.161



ggplot(cleaned_data, aes(median_age, Total_ems))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

cor(cleaned_data$Total_ems, cleaned_data$median_age) ###-0.339



ggplot(cleaned_data, aes(percent_nonwhite, Total_ems))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

cor(cleaned_data$Total_ems, cleaned_data$percent_nonwhite) ###0.496



ggplot(cleaned_data, aes(uninsured, Total_ems))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

cor(cleaned_data$Total_ems, cleaned_data$uninsured) ###0.766


ggplot(cleaned_data, aes(Total_ME, Total_ems))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

cor(cleaned_data$Total_ems, cleaned_data$Total_ME, use = "complete.obs") ###0.856


ggplot(cleaned_data, aes(sqrt(Total_narcan), sqrt(Total_ems)))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

cor(cleaned_data$Total_ems, cleaned_data$Total_narcan, use = "complete.obs") ###0.697
##Running a Poisson regression model on this new dataset
poisson_model <- glm(Total_ems ~ Total_ME + `Non-violent` + Violent + rpopden + 
                       median_age + median_income + low.ed + 
                       percent_nonwhite + uninsured,
                     family = poisson(link = "log"), 
                     data = cleaned_data)
summary(poisson_model)

poisson_model2 <- glm(Total_ems ~ Total_ME + `Non-violent` + Violent + rpopden + 
                       median_age + median_income + low.ed + 
                       percent_nonwhite + uninsured + Total_narcan,
                     family = poisson(link = "log"), 
                     data = cleaned_data)
summary(poisson_model2)
##Based on the results, this is not the right model to run for our data so we are testing a negative binomial
nb_model <- glm.nb(Total_ems ~ Total_ME + `Non-violent` + Violent + rpopden + 
                     median_age + median_income + low.ed + 
                     percent_nonwhite + uninsured, 
                   data = cleaned_data)
summary(nb_model)
vif(nb_model)


nb_model2 <- glm.nb(Total_ems ~ Total_ME + `Non-violent` + Violent + rpopden + 
                     median_age + median_income + low.ed + 
                     percent_nonwhite + uninsured + Total_narcan, 
                   data = cleaned_data)
summary(nb_model2)
vif(nb_model2)
##reducing the explanatory factors with large VIFs
clean_complete <- na.omit(cleaned_data)
names(clean_complete)[names(clean_complete) == "Non-violent"] <- "Non_violent"

clean_complete

write.csv(clean_complete, "C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/model_data.csv", row.names = FALSE)


# Refit the model using only complete cases
nb_model_complete <- glm.nb(
  Total_ems ~ Total_ME + Non_violent + Violent + rpopden + median_age + 
    median_income + low.ed + percent_nonwhite + uninsured,
  data = clean_complete
)

nb_model_complete2 <- glm.nb(
  Total_ems ~ Total_ME + Non_violent + Violent + rpopden + median_age + 
    median_income + low.ed + percent_nonwhite + uninsured + Total_narcan,
  data = clean_complete
)

nb_model_complete3 <- glm.nb(
  Total_ems ~ Total_ME + Violent + 
    median_income + low.ed + uninsured + median_age + Total_narcan,
  data = clean_complete
)


# Now run stepwise AIC
library(MASS)
step_model <- stepAIC(nb_model_complete, direction = "both")

step_model2 <- stepAIC(nb_model_complete2, direction = "both")

# View the result
summary(step_model)
vif(step_model)

summary(step_model2)
vif(step_model2)

summary(step_model3)
# Predicted values (on response scale)
pred_vals <- predict(step_model, type = "response")

# Observed EMS values
obs_vals <- clean_complete$Total_ems

# Plot
plot(pred_vals, obs_vals,
     xlab = "Predicted EMS Calls",
     ylab = "Observed EMS Calls",
     main = "Predicted vs. Observed EMS Calls",
     pch = 19, col = "darkblue")
abline(0, 1, col = "red", lwd = 2)

# Deviance residuals
resid_vals <- residuals(step_model, type = "deviance")

# Plot residuals vs. fitted
plot(pred_vals, resid_vals,
     xlab = "Fitted EMS Calls",
     ylab = "Deviance Residuals",
     main = "Residuals vs. Fitted",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

##Graphing each of the explanatroy variables in the model
plot(ggpredict(step_model, terms = c("Total_ME", "Non-violent", "median_age", "median_income")))

# Predicted values (on response scale)
##using the second model now
pred_vals2 <- predict(step_model2, type = "response")

# Observed EMS values
obs_vals2 <- clean_complete$Total_ems

# Plot
plot(pred_vals2, obs_vals2,
     xlab = "Predicted EMS Calls",
     ylab = "Observed EMS Calls",
     main = "Predicted vs. Observed EMS Calls",
     pch = 19, col = "darkblue")
abline(0, 1, col = "red", lwd = 2)

# Deviance residuals
resid_vals2 <- residuals(step_model2, type = "deviance")

# Plot residuals vs. fitted
plot(pred_vals2, resid_vals2,
     xlab = "Fitted EMS Calls",
     ylab = "Deviance Residuals",
     main = "Residuals vs. Fitted",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

##Graphing each of the explanatroy variables in the second model with 8 variables 
# Individual plots for each variable
p1 <- plot(ggpredict(step_model2, terms = "Total_ME"))
p2 <- plot(ggpredict(step_model2, terms = "Violent"))
p3 <- plot(ggpredict(step_model2, terms = "median_age"))
p4 <- plot(ggpredict(step_model2, terms = "median_income"))
p5 <- plot(ggpredict(step_model2, terms = "low.ed"))
p6 <- plot(ggpredict(step_model2, terms = "percent_nonwhite"))
p7 <- plot(ggpredict(step_model2, terms = "uninsured"))
p8 <- plot(ggpredict(step_model2, terms = "Total_narcan"))

# Combine into a grid

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 3)

