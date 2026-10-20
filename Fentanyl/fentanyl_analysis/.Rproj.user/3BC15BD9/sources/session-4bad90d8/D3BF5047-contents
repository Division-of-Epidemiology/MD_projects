library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

getwd()
data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/Dashboard_ME_Data.csv")

data <- data %>%
  mutate(Quarter = paste0("Q", Quarter),
         Date = yq(paste0(Year, "", Quarter))) %>%
  mutate(Fentanyl = as.numeric(gsub("%", "", Fentanyl)))




plot1 <- ggplot(data, aes(x = Date, y = Fentanyl, group = 1)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  scale_y_continuous(limits = c(0,NA)) +
  labs(
    title = "Fentanyl % Over Time (Continuous)",
    x = "Quarter",
    y = "Fentanyl %"
  ) +
  theme_minimal()

plot1

##Looking for autocorrelation in the data
acf(data$Fentanyl, main="ACF of Fentanyl %")
pacf(data$Fentanyl, main="PACF of Fentanyl %")

##Testing out different regression models
data <- data %>%
  arrange(Date) %>%
  mutate(
    time_index = row_number()
  )

glm_model <- glm(
  Fentanyl/100 ~ time_index + Quarter,
  family = quasibinomial,
  weights = Fatal_count,
  data=data
)


summary(glm_model)

##Generate model predictions
pred <- predict(glm_model, type = "link", se.fit = TRUE)
data$fit  <- glm_model$family$linkinv(pred$fit)
data$lower <- glm_model$family$linkinv(pred$fit - 1.96 * pred$se.fit)
data$upper <- glm_model$family$linkinv(pred$fit + 1.96 * pred$se.fit)


ggplot(data, aes(x=time_index)) +
  geom_point(aes(y=Fentanyl/100), color="black") +
  geom_line(aes(y=fit), color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = 0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Predicted Fentanyl % Over Time",
    y = "Fentanyl %",
    x = "Date"
  ) + theme_minimal()

##Trying abother model
library(segmented)

lin_mod <- lm(Fentanyl/100 ~ time_index, data=data)
seg_mod <- segmented(lin_mod, seg.Z = ~ time_index)

plot(seg_mod)
summary(seg_mod)


##Figure out where the break happened
break_index <- round(seg_mod$psi[2])
break_date <- data$Date[break_index]

break_year <- data$Year[break_index]
break_quarter <- data$Quarter[break_index]




theme_cdc <- function() {
  theme_minimal(base_size = 16, base_family = "sans") +
    theme(
      panel.grid.major = element_line(color = "grey85", size = 0.3),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 18, hjust = 0),
      plot.subtitle = element_text(size = 14, hjust = 0),
      axis.text = element_text(color = "grey20"),
      plot.margin = margin(15, 15, 15, 15),
      legend.position = "none"
    )
}

# Build plot with breakpoint
  ggplot(data, aes(x = Date)) +
  geom_point(aes(y = Fentanyl/100),
             color = "black", size = 2, alpha = 0.8) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = "grey80",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = fit),
    color = "#005F9E",        # CDC blue
    size = 1.4
  ) +
  geom_vline(
    xintercept = as.numeric(break_date),
    linetype = "dashed",
    color = "#D62728",
    size = 1
  ) +
  annotate(
    "text",
    x = break_date + 60,
    y = max(data$Fentanyl/100) + 0.02,
    label = paste0("Trend change\n", break_year, " ", break_quarter),
    color = "#D62728",
    fontface = "bold",
    hjust = 0,
    vjust = 0,
    size = 5
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Fentanyl Involvement Over Time",
    subtitle = "Breakpoint indicates significant change in trend",
    x = "Date",
    y = "Fentanyl %"
  ) +
  theme_cdc()
