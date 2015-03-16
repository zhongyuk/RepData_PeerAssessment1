
activity_CSV <- unzip("activity.zip")

## read data into R workspace
activity_data <- read.csv(activity_CSV, header=TRUE)
colnames(activity_data)

## convert variable date from factor to Date
activity_data$date <- as.Date(activity_data$date)




library(dplyr)

## Calculate the total number of steps taken per day
## ifelse line is used to deal with missing values:
## If date X, its steps' values are ALL missing, sum of this date should be NA
## If date X, its steps' values are partially missing, use sum FUN to compute the sum ignoring NA values
StepperDay <- group_by(activity_data, date) %>% summarize(
    ifelse(sum(!is.na(steps))>0, sum(steps, na.rm=TRUE), sum(steps, na.rm=FALSE)))

## Change column names
colnames(StepperDay) <- c("date", "total")

## Plot the histogram of total steps per day
hist(StepperDay$total, breaks = 10, col = "red",
     main = "Histogram of Total Steps Taken Per Day",
     xlab = "Total Steps Taken Per Day")

## Calculate and auto print the mean and median of the total steps per day
print("The mean of the total number of steps taken per day is:", quote = FALSE)
mean(StepperDay$total, na.rm = TRUE)

print("The median of the total number of steps taken per day is:", quote = FALSE)
median(StepperDay$total, na.rm = TRUE)



## Calcuate the steps of each interval averaged across all days
## ifelse line is used to deal with missing values:
## For a particular interval, if ALL the step values on that interval are missing,
## then the avearaged step counts on that interval is NA;
## if the step values on that interval are PARTIALLY missing,
## then compute the mean across all days ignoring the missing value.
Step_Interval <- group_by(activity_data, interval) %>% summarize(
    ifelse(sum(!is.na(steps))>0, mean(steps, na.rm=TRUE), mean(steps, na.rm=FALSE)))

## Rename column names
colnames(Step_Interval) <- c("interval", "mean")

## Create a time series, which starts from 2012-10-01 00:00:00 UTC,
## ends at 2012-10-01 23:55:00 UTC, each element is 5 mins apart. Total: 288 time points
## Note: the specific date and time zone dooesn't matter for the purpose of plotting a correct
## time series figure, it's set for the purpose of create the desired time series correctly
basedate <- activity_data$date[1]
basedatePOSIX <- as.POSIXlt(basedate, origin = "1970-01-01 00:00.00 UTC")
basedateNumeric <- as.numeric(basedatePOSIX)
basedateEnd <- basedateNumeric + (length(Step_Interval$interval)-1)*300
interval_ts_Numeric <- seq(basedateNumeric, basedateEnd, 300)
interval_ts <- as.POSIXlt(interval_ts_Numeric, tz="UTC", origin="1970-01-01 00:00.00 UTC")

## Plot time series: average daily activity pattern
plot(interval_ts, Step_Interval$mean, type="l",
     main="Average Daily Activity Pattern", xlab="time in a day", 
     ylab="steps per 5 mins(averaged across measured days)", lwd=2)

## The 5-mins interval contains the maximum number of steps
ind <- which(Step_Interval$mean == max(Step_Interval$mean))
#interval_ts[ind]
print("The 5-mins interval which contains the maximum number of steps is:",
      quote = FALSE)
Step_Interval$interval[ind]






## 1. Calculate and report total number of missing values
ind_NA <- is.na(activity_data$steps)
count_NA <- sum(ind_NA)
print("The number of rows with mising value NA is:", quote = FALSE)
count_NA

## 2. Filling missing data: insert the mean (averaged across all days) step counts 
## of the corresponding 5-mins interval to the missing value

interval_NA <- activity_data$interval[which(ind_NA)]
step_impute <- Step_Interval$mean[match(interval_NA,Step_Interval$interval)]

## 3. Create a new dataset equals to original one with missing data filled
copy_activity <- activity_data
copy_activity$steps[which(ind_NA)] <- step_impute

## 4. Histogram of total numbers per day, report and
## compare the mean and median with the results got in first section
copy_StepperDay <- group_by(copy_activity, date) %>% summarize(sum(steps, na.rm=TRUE))

## Change column names
colnames(copy_StepperDay) <- c("date", "total")

## Plot the histogram of total steps per day
hist(copy_StepperDay$total, breaks = 10, col = "red",
     main = "Histogram of Total Steps Taken Per Day, Missing value imputed",
     xlab = "Total Steps Taken Per Day")

## Calculate and auto print the mean and median of the total steps per day
print("The mean of the total number of steps taken per day after imputing
      missing values:", quote = FALSE)
mean(copy_StepperDay$total)

print("The median of the total number of steps taken per day after imputing
      missing values:", quote = FALSE)
median(copy_StepperDay$total)






## Mutate a variable called weekdayInfo, with two levels of: weekday, weekend
copy_activity <- mutate(copy_activity, weekdayInfo = 
                   factor((sapply(date, weekdays, abbreviate=TRUE) %in% c("Sat", "Sun")),
               labels = c("weekday","weekend")))


## Split copy_activity into two different dataframe by weekdayInfo
Temp <- split(copy_activity, copy_activity$weekdayInfo)
act_weekday <- Temp[[1]]
act_weekend <- Temp[[2]]

## Calcuate the steps of each interval averaged across all weekdays/weekends
## Note: since already imputed missing values, no need to set na.rm = TRUE
Step_weekday <- group_by(act_weekday, interval) %>% summarize(mean(steps))
colnames(Step_weekday) <- c("interval","mean")
Step_weekend <- group_by(act_weekend, interval) %>% summarize(mean(steps))
colnames(Step_weekend) <- c("interval","mean")

## Plot time series: average daily activity pattern - weekday vs weekend
par(mfrow=c(2,1))
plot(interval_ts, Step_weekend$mean, type="l",
     main="Average Daily Activity Pattern at Weekends", xlab="time in a day", 
     ylab="Number of steps per 5 mins", lwd=2)

plot(interval_ts, Step_weekday$mean, type="l",
     main="Average Daily Activity Pattern at Weekdays", xlab="time in a day", 
     ylab="Number of steps per 5 mins", lwd=2)
