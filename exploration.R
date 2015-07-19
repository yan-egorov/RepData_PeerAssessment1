library(lattice)

data <- read.csv(file = "activity.csv")
head(data)
str(data)

steps_by_day <- aggregate(steps ~ date, data = data, FUN = sum)
str(steps_by_day)
hist(steps_by_day$steps,breaks = 20)
mean(steps_by_day$steps)
median(steps_by_day$steps)

steps_by_time <- aggregate(steps ~ interval, data = data, FUN = mean)
plot(x = steps_by_time$interval, 
     y = steps_by_time$steps, 
     type ='l')
max_steps_arg = which.max(steps_by_time$steps)
max_steps_time <- steps_by_time$interval[max_steps_arg]

summary(data$steps)["NA's"]
data_tidy <- data
for (iter in 1:nrow(data_tidy))
    {
    if (is.na(data_tidy$steps[iter]))
        {
        time_interval <- data_tidy$interval[iter]
        mean_for_time <- steps_by_time[steps_by_time$interval==time_interval,"steps"]
        data_tidy$steps[iter] <- mean_for_time
        cat(iter,time_interval,mean_for_time,'\n', sep=' ### ')
        } 
    }
summary(data_tidy$steps)

steps_by_day_filled <- aggregate(steps ~ date, data = data_tidy, FUN = sum)
hist(steps_by_day_filled$steps,breaks = 20)
mean(steps_by_day$steps)
median(steps_by_day$steps)

data_tidy$weekday <- weekdays(as.Date(data_tidy$date))
data_tidy$weekends <- ifelse(data_tidy$weekday == "Суббота" | 
                             data_tidy$weekday == "Воскресенье", "Weekend", "Weekday")

steps_by_time_weekend_flag <- aggregate(data_tidy$steps, 
                                        by = list(data_tidy$interval, data_tidy$weekends), 
                                        FUN=mean)
colnames(steps_by_time_weekend_flag) <- c("interval", "weekend_flag", "steps")
xyplot(steps ~ interval | weekend_flag, 
       steps_by_time_weekend_flag, 
       type = "l", 
       layout = c(1,2), 
       xlab = "Interval", 
       ylab = "Number of steps", 
       main = "Activity Patterns on Weekends and Weekdays", 
       col = "steelblue")
