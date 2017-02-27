library(dplyr)
library(lattice)

#Load the data
activity <- read.csv("activity.csv")


# Total number of steps per day
total_steps <- activity %>%
        group_by(date) %>%
        filter(!is.na(steps)) %>%
        summarize(steps = sum(steps,na.rm=T))

##Make histogram
hist(total_steps$steps, breaks=25, xlim=c(0, 25000), col = "green", xlab = "Total steps", ylab = "Frequency", main = "Total number of steps per day")

##Calculate and report the mean and median total # steps per day
mean_steps <- mean(total_steps$steps, na.rm=T)
mean_steps

median_steps <- median(total_steps$steps, na.rm=T)
median_steps


#Average daily activity pattern
daily_act <- activity %>%
        group_by(interval) %>%
        filter(!is.na(steps)) %>%
        summarize(ave_steps = mean(steps,na.rm=T))

##Make time-series plot 
plot(daily_act$interval, daily_act$ave_steps, col="green", type = "l", lwd = 2, xlab="5min Interval", ylab = "Average number steps")

##5-min interval on average that contains maximum number of steps
max_int <- daily_act[which.max(daily_act$ave_steps),]
max_int


#Imputing missing values
## Calculate and report total # NAs:
total_na <- sum(is.na(activity$steps))
total_na

##Devise strategy for filling in the missing values
# Use mean for that 5-min interval

## Create new dataset equal to original dataset but with the missing data filled in
activity2 <- activity %>%
        group_by(interval) %>%
        mutate(imp_steps = replace(steps, is.na(steps), mean(steps,na.rm=T)))


## Calculate total number of steps per day and make histagram
total_imp_steps <- activity2 %>%
        group_by(date) %>%
        summarize(steps = sum(imp_steps,na.rm=T))
total_imp_steps

hist(total_imp_steps$steps, breaks=25, xlim=c(0, 25000), col = "green", xlab = "Total steps", ylab = "Frequency", main = "Total steps per day with imputed missing values")

##Calculate and report the mean and median total # steps per day
mean_imp_steps <- mean(total_imp_steps$steps, na.rm=T)
mean_imp_steps

median_imp_steps <- median(total_imp_steps$steps, na.rm=T)
median_imp_steps

#Impact of imputing missing data
summary(total_steps)
summary(total_imp_steps)
#Mean and median values are almost the same, but the quantiles are significantly different.


# differences in activity patterns between weekdays and weekends
## Create a new factor variable in the dataset  with 2 levels: weekday and weekend. Weekdays() function may be helpful.
activity2$date <- as.Date(activity2$date)

activity2$day_type = ifelse(weekdays(activity2$date)=="Saturday" | weekdays(activity2$date)=="Sunday", "Weekend", "Weekday")

## Make panel plot containing time series plot of the 5-min interval (x-axis) and the avergae number of steps taken, averaged across all weekday days or weekend days (y-axis).
Interval<- activity2 %>%
        group_by(interval, day_type) %>%
        summarise(avg_steps = mean(imp_steps, na.rm=TRUE))

Interval$day_type <- as.factor(Interval$day_type)

xyplot(avg_steps ~ interval | day_type, data = Interval, type = "l", col = "green", layout = c(1,2))
# During the weekday the test object is more active earlier in the day while during the weekend activity is spread out thoughout the day. Probably test object exercises before work during the weekdays.

