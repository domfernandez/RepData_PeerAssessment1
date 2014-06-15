# Reproducible Research: Peer Assessment 1
This Project uses data collected from a personal activity monitoring device. 

This device collects data at 5 minute intervals throughout the day, for two months - October and November of 2012, of an anonymous person.

The information recorded at 5-minute intervals is the number of steps.

## Loading and preprocessing the data

1. If the data-file does not exist, it is downloaded as a zipped-file.
... Zipped-file is uncompressed and read as a data frame.
3. The interval column is converted to factor type.
4. The date column is converted to Date type.
5. The data is examined by using summary and str methods on it.


```r
library(ggplot2)  # we shall use ggplot2 for plotting figures

# download and read the data, convert columns for convenience
read_data <- function() {
    file_name = "activity.zip"
    dwld_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    if (!file.exists(file_name)) {
        download.file(dwld_url, destfile = file_name, method = "curl")
    }
    con <- unz(file_name, "activity.csv")
    data_table <- read.csv(con, header = T, colClasses = c("numeric", "character", 
        "numeric"))
    data_table$interval <- factor(data_table$interval)
    data_table$date <- as.Date(data_table$date, format = "%Y-%m-%d")
    data_table
}
data_table <- read_data()
```


## What is mean total number of steps taken per day?

A Histogram is plotted for the total number of steps per day (steps/day) - at intervals of 1500 steps.

Mean and Median are also plotted for the "daily total steps".


```r
daily_steps <- function(data_tablel) {
    steps_per_day <- aggregate(steps ~ date, data_table, sum)
    colnames(steps_per_day) <- c("date", "steps")
    steps_per_day
}

plot_steps_per_day <- function(steps_per_day, mean_steps, median_steps) {
    col_labels = c(paste("Mean:", mean_steps), paste("Median:", median_steps))
    cols = c("green", "yellow")
    
    ggplot(steps_per_day, aes(x = steps)) + geom_histogram(fill = "steelblue", 
        binwidth = 1500) + geom_point(aes(x = mean_steps, y = 0, color = "green"), 
        size = 4, shape = 15) + geom_point(aes(x = median_steps, y = 0, color = "yellow"), 
        size = 4, shape = 15) + scale_color_manual(name = element_blank(), labels = col_labels, 
        values = cols) + labs(title = "Histogram of Steps Taken per Day", x = "Number of Steps", 
        y = "Count") + theme_bw() + theme(legend.position = "bottom")
}

steps_per_day <- steps_per_day(data_table)
```

```
## Error: could not find function "steps_per_day"
```

```r
mean_steps = round(mean(steps_per_day$steps), 2)
```

```
## Error: object 'steps_per_day' not found
```

```r
median_steps = round(median(steps_per_day$steps), 2)
```

```
## Error: object 'steps_per_day' not found
```

```r
plot_steps_per_day(steps_per_day, mean_steps, median_steps)
```

```
## Error: object 'mean_steps' not found
```


## What is the average daily activity pattern?

A plot of the average daily pattern of the number of steps is now plotted against the interval number. The interval that clocks the maximum number of steps on the average is also marked.



```r
steps_per_interval <- function(data_table) {
    steps_pi <- aggregate(data_table$steps, by = list(interval = data_table$interval), 
        FUN = mean, na.rm = T)
    # convert to integers for plotting
    steps_pi$interval <- as.integer(levels(steps_pi$interval)[steps_pi$interval])
    colnames(steps_pi) <- c("interval", "steps")
    steps_pi
}

plot <- function(steps_per_interval, max_step_interval) {
    col_labels = c(paste("Interval with Maximum Activity: ", max_step_interval))
    cols = c("red")
    
    ggplot(steps_per_interval, aes(x = interval, y = steps)) + geom_line(color = "steelblue", 
        size = 1) + geom_point(aes(x = max_step_interval, y = 0, color = "red"), 
        size = 4, shape = 15) + scale_color_manual(name = element_blank(), labels = col_labels, 
        values = cols) + labs(title = "Average Daily Activity Pattern", x = "Interval", 
        y = "Number of steps") + theme_bw() + theme(legend.position = "bottom")
}

steps_per_interval <- calc_steps_per_interval(data_table)
```

```
## Error: could not find function "calc_steps_per_interval"
```

```r
max_step_interval <- steps_per_interval[which.max(steps_per_interval$steps), 
    ]$interval
```

```
## Error: object of type 'closure' is not subsettable
```

```r

plot_activity_pattern(steps_per_interval, max_step_interval)
```

```
## Error: could not find function "plot_activity_pattern"
```


The **

```

Error in eval(expr, envir, enclos) : object 'max_step_interval' not found

```

<sup>th</sup> interval** has the maximum activity on the average.


## Imputing missing values

Missing values are populated with the mean value - at the same interval across days.

Choose to do this because of the assumption that activities usually follow a daily pattern.


```r
impute_means <- function(data_table, defaults) {
    na_indices <- which(is.na(data_table$steps))
    defaults <- steps_per_interval
    na_replacements <- unlist(lapply(na_indices, FUN = function(idx) {
        interval = data_table[idx, ]$interval
        defaults[defaults$interval == interval, ]$steps
    }))
    imp_steps <- data_table$steps
    imp_steps[na_indices] <- na_replacements
    imp_steps
}
complete_tbl <- data.frame(steps = impute_means(data_table, steps_per_interval), 
    date = data_table$date, interval = data_table$interval)
```

```
## Error: object of type 'closure' is not subsettable
```


The "completed" dataset is now Summarized with the updated values.

```r
summary(complete_tbl)
```

```
## Error: object 'complete_tbl' not found
```


With the imputed dataset, below is a histogram of the daily total number of steps taken, plotted with a bin interval of 1500 steps. Also marked on the plot are the mean and median of the daily total steps.


```r
complete_steps_per_day <- daily_steps(complete_tbl)
complete_mean_steps = round(mean(complete_steps_per_day$steps), 2)
complete_median_steps = round(median(complete_steps_per_day$steps), 2)
plot_steps_per_day(complete_steps_per_day, complete_mean_steps, complete_median_steps)
```

```
## Error: object 'mean_steps' not found
```


Reviewing the work done, it is observed that mean value remains unchanged - but the median value shifts closer to the mean.

## Are there differences in activity patterns between weekdays and weekends?

We do this comparison with the table with filled-in missing values.

1. Augment the table with a column that indicates the day of the week
2. Subset the table into two parts - weekends (Saturday and Sunday) and weekdays (Monday through Friday).
3. Tabulate the average steps per interval for each dataset.
4. Plot the two datasets side by side for comparison.


```r
calc_day_of_week_data <- function(data_table) {
    data_table$weekday <- as.factor(weekdays(data_table$date))
    weekend_data <- subset(data_table, weekday %in% c("Saturday", "Sunday"))
    weekday_data <- subset(data_table, !weekday %in% c("Saturday", "Sunday"))
    
    weekend_spi <- calc_steps_per_interval(weekend_data)
    weekday_spi <- calc_steps_per_interval(weekday_data)
    
    weekend_spi$dayofweek <- rep("weekend", nrow(weekend_spi))
    weekday_spi$dayofweek <- rep("weekday", nrow(weekday_spi))
    
    day_of_week_data <- rbind(weekend_spi, weekday_spi)
    day_of_week_data$dayofweek <- as.factor(day_of_week_data$dayofweek)
    day_of_week_data
}
plot_day_of_week_comparison <- function(dow_data) {
    ggplot(dow_data, aes(x = interval, y = steps)) + geom_line(color = "steelblue", 
        size = 1) + facet_wrap(~dayofweek, nrow = 2, ncol = 1) + labs(x = "Interval", 
        y = "Number of steps") + theme_bw()
}
day_of_week_data <- calc_day_of_week_data(complete_tbl)
```

```
## Error: object 'complete_tbl' not found
```

```r
plot_day_of_week_comparison(day_of_week_data)
```

```
## Error: object 'day_of_week_data' not found
```


Is is observed that "activity" on the weekends tends to be more spread out over the day - when compared to the weekdays. 

The reason for this could be due to the fact that activities on weekdays are usually routine when compared to the weekends.
