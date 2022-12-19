# Loading the data
activity <- read_csv("Reproducible Research/week2/activity.csv")
activity$date <- as.Date(x=activity$date, format = "%Y-%m-%d")
original_data <- activity
activity<- na.omit(activity)

# mean total number of steps taken per day
total_steps_per_day <- aggregate(steps ~ date, data = activity, FUN = sum)
hist(total_steps_per_day$steps,
     main = "Total number of steps taken each day",
     xlab = "Number of steps per day",
     col = "blue",
     breaks = 30)

# mean number steps
mean_number_steps <- mean(total_steps_per_day$steps)
mean_number_steps

#median number steps
median_number_steps <- median(total_steps_per_day$steps)
median_number_steps

#average daily activity pattern
average_steps_per_interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(x = average_steps_per_interval$interval,
     y = average_steps_per_interval$steps,
     type = "l",
     col = "blue",
     xlab = "The 5-minutes interval",
     ylab = "Average number of steps taken across all days",
     main = "The average daily activity pattern")

five_min_interval <- average_steps_per_interval$interval[which.max(average_steps_per_interval$steps)]

#missing values
na_steps <- sum(is.na(original_data$steps))
average_for_5min_interval <- aggregate(steps ~ interval, data = original_data, FUN = mean, na.rm = TRUE)
fillNA_data <- original_data
is_na_steps <- is.na(original_data$steps)
aux <- na.omit(subset(average_for_5min_interval, interval == original_data$interval[is_na_steps]))
fillNA_data$steps[is_na_steps] <- aux[, 2]
na_steps_fillNA <- sum(is.na(fillNA_data))
steps_per_day_noNA <- aggregate(steps ~ date, data = fillNA_data, FUN = sum, na.rm = TRUE)

#histogram total number of steps taken per day without missing values
hist(steps_per_day_noNA$steps,
     main = "Total number of steps taken each day (without missing values)",
     xlab = "Number of steps per day",
     col = "blue",
     breaks = 30)
mean_steps_per_day <- mean(steps_per_day_noNA$steps)
mean_steps_per_day
median_steps_per_day <- median(steps_per_day_noNA$steps)
median_steps_per_day
summary(total_steps_per_day$steps)
summary(steps_per_day_noNA$steps)

#diffrence in pattern between weekdays and weekends
weekdays_values = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
date_type <- ifelse(
  weekdays(fillNA_data$date) %in% weekdays_values,
  'weekday',
  'weekend')
fillNA_data$day <- factor(x = date_type)

average_steps_per_weekday <- aggregate(steps ~ interval + day, data = fillNA_data, FUN = mean, na.rm = TRUE) 
library(ggplot2)
ggplot(average_steps_per_weekday, aes(interval, steps, color = day)) +
  geom_line() +
  facet_grid(day ~ .) +
  xlab('5-minute interval') +
  ylab('Average number of steps') +
  ggtitle('Activity pattern by the week of the day ')

