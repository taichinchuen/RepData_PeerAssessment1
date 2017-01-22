## Loading and preprocessing the data

# 1. Load the data (i.e. read.csv())

activitydata <- read.csv("activity.csv")
head(activitydata)
str(activitydata)

# 2. Process/transform the data (if necessary) into a format suitable for your analysis

activitydata$date <- as.Date(activitydata$date, "%Y-%m-%d")

str(activitydata)
summary(activitydata)

## What is mean total number of steps taken per day?

# 1. Calculate the total number of steps taken per day

totalsteps<- aggregate(steps ~ date, data = activitydata, sum, na.rm = TRUE)
head(totalsteps)

# 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

hist(totalsteps$steps, main = "Histogram of The Total Steps Taken per day", xlab = "Number of Steps", ylab="Number of Days", breaks = 10, col = "blue")

# 3. Calculate and report the mean and median of the total number of steps taken per day

mean(totalsteps$steps)
median(totalsteps$steps)

## What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

averagesteps <- aggregate(steps~interval, activitydata, mean)
with(averagesteps, plot(interval, steps, type = "l", xlab = "5-minute Interval", ylab = "Average Steps", main = "Average Steps Across All Days"))

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxaveragesteps <- averagesteps[which.max(averagesteps$steps), 1]

## Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

missingdata <- is.na(activitydata[ , 1])  
table(missingdata)
nummissingdata <- sum(missingdata)
nummissingdata

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

averagesteps <- aggregate(steps~interval, activitydata, mean)

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

newdata <- activitydata

for (i in 1:nrow(newdata)) {
  if (is.na(newdata$steps[i])) {
    newdata$steps[i] <- averagesteps[which(newdata$interval[i] == averagesteps$interval), ]$steps
  }
}

sum(is.na(newdata))

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

newtotalsteps <- aggregate(steps ~ date, data = newdata, sum, na.rm = TRUE)
head(newtotalsteps)

hist(newtotalsteps$steps, xlab="Number of Steps", ylab="Number of Days", main="Number of Steps Taken per day with Missing Values Imputed", breaks = 10, col = "blue")

mean(newtotalsteps$steps)
median(newtotalsteps$steps)

## Are there differences in activity patterns between weekdays and weekends?

# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

class(newdata$date)
weekdata <- ifelse(weekdays(newdata$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
newdata$weekdata <- as.factor(weekdata)
str(newdata)
head(newdata)

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

newaveragesteps <- aggregate(steps~interval+weekdata, newdata, mean)

library(lattice)
xyplot(steps ~ interval | weekdata,
       layout = c(1, 2),
       xlab = "Interval",
       ylab = "Number of Steps",
       main = "Average Steps Across Weekday Days or Weekend Days",
       type = "l",
       data = newaveragesteps)
