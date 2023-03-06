# Loading data
data <- read.csv("activity.csv")

# mean total number of steps taken per day
library(ggplot2)
library(dplyr)

total_steps <- data %>%
  group_by(date) %>%
  summarise(daily_steps = sum(steps, na.rm = TRUE))

ggplot(total_steps, aes(daily_steps)) + geom_histogram(binwidth = 2000) +
  xlab("Total number of steps taken each day") + 
  ylab("Frequency")

# mean and median
mean = mean(total_steps$daily_steps, na.rm=TRUE)
median = median(total_steps$daily_steps, na.rm=TRUE)