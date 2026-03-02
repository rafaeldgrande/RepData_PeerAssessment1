---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


``` r
# Load required libraries
library(ggplot2)
library(dplyr)

# Load the data
activity <- read.csv("activity.csv")

# Convert date column to Date type
activity$date <- as.Date(activity$date)

# Preview the data
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

``` r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

---

## What is mean total number of steps taken per day?


``` r
# Calculate total steps per day (ignoring NAs)
steps_per_day <- activity %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))

# 1. Histogram of total steps per day
ggplot(steps_per_day, aes(x = total_steps)) +
  geom_histogram(binwidth = 2000, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Total Steps per Day",
       x = "Total Steps per Day",
       y = "Frequency") +
  theme_minimal()
```

![](PA1_template_files/figure-html/steps_per_day-1.png)<!-- -->

``` r
# 2. Mean and median
mean_steps   <- mean(steps_per_day$total_steps, na.rm = TRUE)
median_steps <- median(steps_per_day$total_steps, na.rm = TRUE)

cat("Mean total steps per day:  ", round(mean_steps, 2), "\n")
```

```
## Mean total steps per day:   9354.23
```

``` r
cat("Median total steps per day:", median_steps, "\n")
```

```
## Median total steps per day: 10395
```

The **mean** total number of steps taken per day is **9354.23** and the **median** is **10395**.

---

## What is the average daily activity pattern?


``` r
# Calculate average steps per 5-minute interval across all days
interval_avg <- activity %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

# 1. Time series plot
ggplot(interval_avg, aes(x = interval, y = avg_steps)) +
  geom_line(color = "steelblue") +
  labs(title = "Average Daily Activity Pattern",
       x = "5-Minute Interval",
       y = "Average Number of Steps") +
  theme_minimal()
```

![](PA1_template_files/figure-html/daily_activity-1.png)<!-- -->

``` r
# 2. Interval with maximum average steps
max_interval <- interval_avg$interval[which.max(interval_avg$avg_steps)]
cat("5-minute interval with maximum average steps:", max_interval, "\n")
```

```
## 5-minute interval with maximum average steps: 835
```

The 5-minute interval that contains the maximum number of steps on average is **interval 835**.

---

## Imputing missing values


``` r
# 1. Total number of missing values
total_na <- sum(is.na(activity$steps))
cat("Total number of rows with NA:", total_na, "\n")
```

```
## Total number of rows with NA: 2304
```

``` r
# 2 & 3. Strategy: fill NAs with the mean for that 5-minute interval
activity_imputed <- activity %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps)) %>%
  ungroup()

# Verify no NAs remain
cat("NAs after imputation:", sum(is.na(activity_imputed$steps)), "\n")
```

```
## NAs after imputation: 0
```

``` r
# 4. Histogram of total steps per day with imputed data
steps_per_day_imputed <- activity_imputed %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

ggplot(steps_per_day_imputed, aes(x = total_steps)) +
  geom_histogram(binwidth = 2000, fill = "darkorange", color = "white") +
  labs(title = "Histogram of Total Steps per Day (Imputed Data)",
       x = "Total Steps per Day",
       y = "Frequency") +
  theme_minimal()
```

![](PA1_template_files/figure-html/imputing-1.png)<!-- -->

``` r
# Mean and median after imputation
mean_steps_imp   <- mean(steps_per_day_imputed$total_steps)
median_steps_imp <- median(steps_per_day_imputed$total_steps)

cat("Mean total steps per day (imputed):  ", round(mean_steps_imp, 2), "\n")
```

```
## Mean total steps per day (imputed):   10766.19
```

``` r
cat("Median total steps per day (imputed):", round(median_steps_imp, 2), "\n")
```

```
## Median total steps per day (imputed): 10766.19
```

- **Total missing values:** 2304  
- **Mean after imputation:** 1.076619\times 10^{4} (was 9354.23)  
- **Median after imputation:** 1.076619\times 10^{4} (was 10395)  

Imputing missing values with interval means increases both the mean and median, and the distribution becomes more symmetric and centered. Days that previously had 0 total steps (because all values were `NA`) now have the expected average total, which removes the left-side spike in the histogram.

---

## Are there differences in activity patterns between weekdays and weekends?


``` r
# 1. Create weekday/weekend factor variable
activity_imputed <- activity_imputed %>%
  mutate(day_type = factor(
    ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
  ))

# 2. Panel plot: average steps by interval and day type
interval_daytype <- activity_imputed %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps), .groups = "drop")

ggplot(interval_daytype, aes(x = interval, y = avg_steps)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ day_type, nrow = 2) +
  labs(title = "Average Steps: Weekdays vs Weekends",
       x = "5-Minute Interval",
       y = "Average Number of Steps") +
  theme_minimal()
```

![](PA1_template_files/figure-html/weekday_weekend-1.png)<!-- -->

- On **weekdays**, activity peaks sharply in the morning (around interval 835), and then remains relatively low throughout the day.  
- On **weekends**, activity is more distributed throughout the day.
