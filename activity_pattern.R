## Make a histogram of the total number of steps taken each day

interval_steps <- data %>% 
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm =TRUE))

ggplot(data=interval_steps, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute intervals") +
  ylab("Average number of steps taken")


## Imputing missing values
missing <- !complete.cases(data)

# impute missing steps with interval averages across days
imputed_data <- data %>%
  mutate(
    steps = case_when(
      is.na(steps) ~ interval_steps$steps[match(data$interval, interval_steps$interval)],      
      TRUE ~ as.numeric(steps)
    ))

imputed_total_steps <- imputed_data %>% group_by(date) %>% summarise(daily_steps = sum(steps))

ggplot(imputed_total_steps, aes(daily_steps)) + 
  geom_histogram(binwidth = 2000) + 
  xlab("Total number of steps taken each day") + 
  ylab("Frequency")


## mean and median
imputed_mean = mean(imputed_total_steps$daily_steps, na.rm=TRUE)
imputed_median = median(imputed_total_steps$daily_steps, na.rm=TRUE)

## the difference of the means and medians between imputed and original data 
mean_diff <- imputed_mean - mean 
median_diff <- imputed_median - median