# Project 1
## Reproducable Data - repdata-014
## Author: Kim Kyllesbech Larsen
### May 2015

# LIBARIES UZED

if("stargazer" %in% rownames(installed.packages()) == FALSE) {install.packages("stargazer")}
library(stargazer)

# DEFINED FUNCTIONS

save_png <- function(n,d) {
        
## Function `save_png` save a png formatted chart named `n`
## to specified directory 'd`
        
        setwd(d)
        
        dev.copy(png,n)
        dev.off()
        
        setwd("../")
}


UnZipped <- function(DataDir, fileUrl) {
        
## Function `UnZipped` checks;
### Does DataDir already exists.
### IF not then create DataDir and unzip the dataset into it DataDir.

if (file.exists(DataDir) == FALSE) {
        
        dir.create(DataDir)
        setwd(DataDir)
        
        t <- tempfile()

        download.file(fileUrl, t, mode = "wb")
        
        unzip(t)
        unlink(t)
        
        setwd("../")
    }

}


DataExist <- function(DataDir, DataSet) {

## Function DataExist checks;
### Is the dataset indeed included in the DataDir.
### If not unzip and add DataSet to DataDir.
        
setwd(DataDir)

if (file.exists(DataSet) == FALSE) {
        
        t <- tempfile()
        
        download.file(fileUrl, t, mode = "wb")
        
        unzip(t)
        unlink(t)
        
    }

setwd("../")

}

# MAIN ENVIRONMENT

## DATASET DIRECTORY
### Creates directory and extract from the zip file the orinal data which should
### is then stored here.

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
DataDir <- "./dataset" #this is the directory where original data is to be found
DataSet <- "activity.CSV"


## RESULTS DIRECTORY
### Create a directory "results" where resulting files can be stored 

resultsDir <- "./results"
if (file.exists(resultsDir) == FALSE) dir.create(resultsDir)

dataFile <- file.path(DataDir, DataSet)

### check whether the dataframe, read from the dataFile, exists.
### if it exist it does not re-read it.

l <- ifelse(any(ls() %in% "dataPEC"), is.data.frame(get("dataPEC")),FALSE)

if (l == FALSE) { dataActivity <- read.table(dataFile, 
                                             header = TRUE,
                                             sep = ",",
                                             na.strings = "NA", 
                                             colClasses = c("numeric",
                                                            "factor",
                                                            "numeric"))
                  
                  dataActivity$date <- as.Date(dataActivity$date,"%Y-%m-%d")        
}

## PART 1:
## What's the mean total number of steps taken per day

### Total number of steps per day
### Omitting the NA

stepsperDay <- aggregate(steps ~ date, dataActivity, FUN = sum, na.action = na.omit)
lenDayswoNA <- length(stepsperDay$steps)

### As it will be interesting to see whether there are any differences between
### different days during the week (e.g., weekends versus weekdays)
### I add a column whic maps the date into the weekday character description.

weekday <-  weekdays(stepsperDay$date)
stepsperDay <- cbind(stepsperDay,weekday)

### Identifying total number of days in dataset including days without a measurement

stepsperDaywNA <- aggregate(steps ~ date, dataActivity, FUN = sum, na.action = na.pass)
lenDayswNA <- length(stepsperDaywNA$steps)

### Days with no steps identified;

daysENA <- lenDayswNA - lenDayswoNA


### Creating a histogram of the number of steps made over the period;

h <- with(stepsperDay,hist(steps,
               breaks = 10,
               col = "red", 
               main = "Histogram of Steps", 
               xlab = "Steps"))

### Mean & Median (i.e., 50%-pectentile) steps per dat over the period.
### NA has been ommitted.
### using `stargazer` library

stargazer(stepsperDay, type = "text", title = "Steps per day statistics",
          digits = 0,
          summary.logical = TRUE,
          nobs = TRUE,
          mean.sd = TRUE,
          min.max = FALSE,
          median = TRUE,
          flip = TRUE)

## PART 2:
## What's the average daily activity pattern 

stepsperInterval <- aggregate(steps ~ interval, dataActivity, FUN = mean, na.action = na.omit)
t <- with(stepsperInterval,plot(interval,steps, type = "l"))

### Maximum number of `steps` averaged across all days
MaxSteps <- max(stepsperInterval$steps)

### The 5-minute interval containing the maximum number of steps
interval <- stepsperInterval$interval[stepsperInterval$steps==MaxSteps]

### Round MaxSteps to nearest whole integer
atMaxSteps <- round(MaxSteps,0)

### The 5-minute interval at maxium steps 
tb1 <- cbind(interval, atMaxSteps)
print(tb1)

## PART 3
## Inputing missing values

### Number of `NA`'s in the step data/column
intervalswNA <- length(dataActivity$steps[is.na(dataActivity$steps)==TRUE])

### Total number of 5-min intervals with `steps` `NA` (e.g., no measurement)
### and the `NA` equivalent number of days 
tb2 <- cbind(intervalswNA,daysENA)
print(tb2)





