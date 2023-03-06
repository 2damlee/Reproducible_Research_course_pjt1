library(lubridate)

## differences in activity patterns between weekdays and weekends
day_of_week <- imputed_data %>%
  mutate(
    date = ymd(date),
    weekday_or_weekend = case_when(wday(date) %in% 2:6 ~ "Weekday",
                                   wday(date) %in% c(1,7) ~ "Weekend")
  ) %>% select(-date) %>%
  group_by(interval, weekday_or_weekend) %>%
  summarise(
    steps = mean(steps)
  )

## panel plot
ggplot(day_of_week, aes(interval, steps)) + 
  geom_line() + 
  facet_wrap(~weekday_or_weekend, nrow = 2) +
  xlab("5-Minute intervals") + 
  ylab("Average number of steps")

