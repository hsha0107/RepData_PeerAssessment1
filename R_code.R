# read data from file
dtset <- read.csv("activity.csv", header = T)
dtset$date <- as.Date(dtset$date)

# total number of steps taken per day
step_per_day <- aggregate(steps~date, data = dtset, sum)

# histogram of total number of steps each day
hist(step_per_day$steps, main = "histogram of total steps each day", xlab = "total number of steps", breaks = 10)

# mean and median for total number of steps per day
mean(step_per_day$steps)
median(step_per_day$steps)

 
# time series plot of interval & average number of steps, averaged across all days
avgIntervalStep <- aggregate(steps~interval, data = dtset, mean)
with(avgIntervalStep, plot(interval, steps, type = "l", main = "average number of steps of each interval"))

# interval contains the maximum number of steps, averaged across all days
max(avgIntervalStep$steps)
avgIntervalStep$interval[which(avgIntervalStep$steps == max(avgIntervalStep$steps))]

# total number of missing values in the dataset
sum(is.na(dtset))

# new dataset, equal to the original dataset but with missing data filled in
newdtset <- dtset
for(i in 1:nrow(newdtset)){
     if(is.na(newdtset$steps[i])){
          newdtset$steps[i] <- avgIntervalStep$steps[avgIntervalStep$interval == newdtset$interval[i]]
          }
}

# histogram of total number of steps each day
step_per_day1 <- aggregate(steps~date, data = newdtset, sum)
hist(step_per_day1$steps, main = "histogram of total steps each day", xlab = "total number of steps", breaks = 10)

# mean and median for total number of steps per day
mean(step_per_day1$steps)
median(step_per_day1$steps)

# create new factor variable weekday
newdtset$weekday <- "weekday"
newdtset$weekday[which(weekdays(newdtset$date) == "Saturday" | weekdays(newdtset$date) == "Sunday")] <- "weekend"
newdtset$weekday <- as.factor(newdtset$weekday)

# average number of steps taken each interval, averaged across all weekday days or weekend days
avgIntervalStep1 <- aggregate(steps~interval+weekday, data = newdtset, mean)

# time series plot
library(lattice)
xyplot(steps~interval|weekday, data = avgIntervalStep1, type = 'l', layout = c(1,2))

