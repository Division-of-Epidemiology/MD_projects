library(knitr)
library(epitools)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(forecast)
library(tseries)
library(prophet)
library(patchwork)




getwd()
month <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/Age-adjusted rates/age_rates_code/month_breakdown.csv")

month$Month <- factor(month$Month, levels = c("January", "February", "March", "April", "May", "June", "July",
                                              "August", "September", "October", "November", "December"))
##Looking at all years against each other
ggplot(month, aes(x=Month, y=Count, fill=as.factor(Year))) +
  geom_bar(stat="identity", position = "dodge", show.legend = FALSE) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title = "Stacked Bar Graph by Year for Each Month", 
       x = "Month",
       y = "Total Deaths",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

##Breaking it up to look at certain years compared to each other - basicc regression model
filtered_data <- month %>% filter(Year %in% c(2019, 2023, 2024))

ggplot(filtered_data, aes(x=Month, y=Count, fill=as.factor(Year))) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values = c("2019" = "#1f78b4", "2023" = "#33a02c", "2024" = "#e31a1c")) +
  labs(title = "Stacked Bar Graph by Year for Each Month", 
       x = "Month",
       y = "Total Deaths",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))


##Creating a basic regression model with the monthly breakdown to predict 2025
month$Date <- make_date(year=month$Year, month=month$Month)

model <- lm(Count ~ Date, data=month)

future_data <- data.frame(Date=seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by="month"))  
future_data$Predicted_Count <- predict(model, newdata = future_data)  
print(future_data)  
  
ggplot() +
  geom_line(data = month, aes(x = Date, y = Count), color = "blue") +
  geom_point(data = future_data, aes(x = Date, y = Predicted_Count), color = "red") +
  geom_smooth(data=month, aes(x=Date, y=Count), method="lm", color="black", se=FALSE)+
  labs(title = "Death Count Predictions for 2025", x = "Date", y = "Death Count") +
  theme_minimal()  


##random code
month$Year <- as.numeric(as.character(month$Year))
month$Month <- factor(month$Month, levels = month.name)
month$Month <- as.numeric(month$Month)

##using the package Prophet to make a model
prophet_data<- data.frame(ds=month$Date, y=month$Count)
prophet_model <- prophet(prophet_data)


future <- make_future_dataframe(prophet_model, periods = 12, freq = "month")
forecast_prophet <- predict(prophet_model, future)

plot(prophet_model, forecast_prophet)


##plotting original data with predicted data
month$Date <- as.Date(month$Date)
forecast_prophet$ds <- as.Date(forecast_prophet$ds)

original_plot <- ggplot(month, aes(x=Date, y=Count)) +
  geom_line(color="blue") +
  geom_point(color="blue") +
  labs(title = "Original Mortality by Month", x="Date", y="Death Counts") +
  theme_minimal()

forecast_plot <- ggplot(forecast_prophet, aes(x=ds, y=yhat)) +
  geom_line(color="red") +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="lightgray", alpha=0.5)+
  labs(title = "Prophet Forecast for Death Counts", x="Date", y="Death Count") +
  theme_minimal()
  
combined_plot <- ggplot() +
  geom_line(data = month, aes(x = Date, y = Count), color = "blue") +  # Original data
  geom_point(data = month, aes(x = Date, y = Count), color = "blue") +
  geom_line(data = forecast_prophet, aes(x = ds, y = yhat), color = "red") +  # Prophet forecast
  geom_ribbon(data = forecast_prophet, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "lightgray", alpha = 0.5) +  # Confidence intervals
  labs(title = "Death Count and Prophet Forecast", x = "Date", y = "Death Count") +
  theme_minimal()
  
print(combined_plot)



###using josh script for the projections
dod_ts <- ts(month$Count, frequency = 12)

#stationary tests
plot(dod_ts)
acf(dod_ts)
pacf(dod_ts)
adf.test(dod_ts)

###create the model Josh used auto.arima
dod_model <- auto.arima(dod_ts, ic="aic", trace = TRUE)

acf(ts(dod_model$residuals))
pacf(ts(dod_model$residuals))

##create forecast
forecast_jl <- forecast(dod_model, level = c(95), h=5*12)
plot(forecast_jl)


