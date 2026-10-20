##Looking to see if there is a significant change after Kratom polucy in ED visits
library(MASS)



ed_data <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ESSENCE_2326.csv")

policy_date <- as.Date("2026-07-01")

daily_counts <- ed_data %>%
  group_by(Date) %>%
  summarise(ed_visits = n_distinct(PIN), .groups="drop")


daily_counts <- daily_counts %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date) %>%
  mutate(
    time = as.numeric(Date - min(Date)),
    policy = ifelse(Date >= policy_date, 1, 0)
  )


break_index <- which(daily_counts$Date >= policy_date)[1]
policy_time <- daily_counts$time[break_index]


daily_counts <- daily_counts %>%
  mutate(
    time_after = ifelse(Date >= policy_date,
                        time - policy_time,
                        0)
  )


##Running NBM to see if there are points ED visits change over time
model_nb <- glm.nb(ed_visits ~ time + policy + time_after, data = daily_counts)
summary(model_nb)

##looking at predicted vs actual

daily_counts$pred_nb <- predict(model_nb, type = "response")

ggplot(
  daily_counts %>% 
    filter(Date >= as.Date("2025-01-01"),
           Date <= as.Date("2026-12-31")),
  aes(x = Date, y = ed_visits)
) +
  geom_line(aes(y=ed_visits), color = "black", alpha=0.6) +
  geom_line(aes(y=pred_nb), color="red", size = 1) +
  geom_vline(xintercept = policy_date, linetype = "dashed", color = "blue") +
  labs(
    title = "Segmented Regression (Negative Binomial)",
    subtitle = "Observed vs. Fitted ED Visits",
    y = "ED Visits",
    x = "Date"
  )

