library(GGally)
library(rpart)
library(randomForest)
library(MASS)

rm(list = ls())

########################
# Data Import & Cleaning
########################

flightinfo = read.csv("Flight_on_time_HIX.csv")
weather = read.csv("weather.csv")

head(flightinfo)
dim(flightinfo)
head(weather)
dim(weather)

sum(complete.cases(flightinfo)) / nrow(flightinfo)
sum(complete.cases(weather)) / nrow(weather)

# There are no missing values in the flight info, but most entries are
# incomplete for weather
# If there is no precipitation accumulated, instead of NA we should use 0

weather$precipType[weather$precipType == ""] = "none"
weather$precipAccumulation[is.na(weather$precipAccumulation)] = 0

sum(complete.cases(weather)) / nrow(weather)

# It seems that this was the source of missing values, so we don't need to
# worry about those anymore

# We are only interested in CA flights departing from HIX, so we will
# focus on those

flightinfo = flightinfo[flightinfo$Airline == "CA", ]
flightinfo = flightinfo[flightinfo$Origin_Airport == "HIX", ]
weather = weather[weather$airport == "Highland", ]

# We can get rid of these columns now since they are all the same
# (and other repetitive or irrelevant columns)
flightinfo = flightinfo[, c(-1, -5)]
weather = weather[, c(-1, -2, -21)]

# Getting all data columns into the right data type

str(weather)
weather$date = 0
for (i in 1:nrow(weather)) # This would be more efficient without a for loop but I won't spend the time looking for a faster alternative right now
{
  weather$date[i] = substr(weather$time[i], 1, 10)
  weather$time[i] = paste(substr(weather$time[i], 12, 13), substr(weather$time[i], 15, 16), sep = "")
}
weather$time = as.numeric(weather$time)
weather$precipType = as.factor(weather$precipType)
weather$date = as.Date(weather$date)

str(flightinfo)
indices = substr(flightinfo$FlightDate, 3, 3) == "/"
flightinfo$FlightDate[indices] =
  paste(substr(flightinfo$FlightDate[indices], 7, 10),
        substr(flightinfo$FlightDate[indices], 1, 2),
        substr(flightinfo$FlightDate[indices], 4, 5), sep = "-")
flightinfo$FlightDate = as.Date(flightinfo$FlightDate)
flightinfo$Plane_ID = as.factor(flightinfo$Plane_ID)
flightinfo$Destination_Airport = as.factor(flightinfo$Destination_Airport)
flightinfo$Delay_Reason = as.factor(flightinfo$Delay_Reason)

# It would be most helpful to have weather data available with the flight
# data to predict flight tardiness. We need to merge the datasets
# To do this, I need to make another column in both data frames with the
# data and time (at the beginning of the hour, for flights), and merge on
# this
# I will be going off scheduled departure time, because presumably that
# is what would cause delays

weather$timetag = 0
for (i in 1:nrow(weather))
  weather$timetag[i] = paste(weather$date[i], weather$time[i], sep = ";")

flightinfo$timetag = 0
for (i in 1:nrow(flightinfo))
  flightinfo$timetag[i] = paste(flightinfo$FlightDate[i], floor(flightinfo$Scheduled_Departure_Time[i] / 100) * 100, sep = ";")

flightinfo = merge(flightinfo, weather, by = "timetag")

str(flightinfo)

# Final cleaning: let's get rid of irrelevant and repetitive predictors
flightinfo = flightinfo[, c(-1, -18, -36)]
# Let's create a binary variable for a delay
flightinfo$wasdelayed = as.factor(flightinfo$Arrival_Delay_Minutes > 15)

head(flightinfo)

##################
# Data Exploration
##################

# Let's get an idea of how many flights were actually delayed
sum(flightinfo$wasdelayed == TRUE) / nrow(flightinfo)
# About 24% of flights are delayed - the classes are somewhat imbalanced

# Let's look at some distributions
hist(flightinfo$Arrival_Delay_Minutes)
max(flightinfo$Arrival_Delay_Minutes)
# Two of these delays are huge - 1 is almost a year and 1 is almost 5 years
# I will treat these as mistakes and ignore them
flightinfo = flightinfo[flightinfo$Arrival_Delay_Minutes < 460000, ]
hist(flightinfo$Arrival_Delay_Minutes)
hist(flightinfo$Flight_Distance)
hist(flightinfo$Arrival_Taxi)
hist(flightinfo$precipIntensity)
hist(flightinfo$precipProbability)
hist(flightinfo$temperature)
hist(flightinfo$humidity)
hist(flightinfo$windGust)
hist(flightinfo$visibility)

scatterplot = data.frame(cbind(flightinfo$Arrival_Delay_Minutes, flightinfo$precipIntensity, flightinfo$windGust, flightinfo$visibility))
names(scatterplot) = c("Delay", "Precipitation", "Wind", "Visibility")
ggpairs(scatterplot)

# Trends are tough to see, especially since a lot of these variables have
# skewed distributions

# Are any specific planes or destination airports to blame?

planes = levels(flightinfo$Plane_ID)
h = NULL
for (id in planes)
  h = c(h, sum(flightinfo[flightinfo$Plane_ID == id,]$wasdelayed == TRUE) / length(flightinfo[flightinfo$Plane_ID == id,]$wasdelayed))
hist(h)
planes[which(h > 0.8)]
planes[which(h > 0.9)]

h = NULL
for (id in levels(flightinfo$Destination_Airport))
  h = c(h, sum(flightinfo[flightinfo$Destination_Airport == id,]$wasdelayed == TRUE) / length(flightinfo[flightinfo$Destination_Airport == id,]$wasdelayed))
hist(h)

# There are some planes with a higher delay rate, but not many, and they
# are not causing the majority of delays. It is better to look at all planes.
# Similarly, there are no particularly troublesome airports.

#####################
# Predictive Modeling
#####################

# Assumption: I'm going to assume that information about the departure and
# arrival (delay, taxi time, and such) aren't useful for predicting delays
# since they're basically real-time data (for example, if you're late
# taking off, then it's rather obvious and not very helpful that you'll
# be late landing).
sum(flightinfo$Delay_Reason == "Weather")
# This is a problem, since very few flights are actually delayed due to
# weather but this is pretty much all the data we have (we don't have any
# data that would help us predict whether the aircraft will be late, or
# whether there will be a security issue, etc.)

# Decision tree to look at important variables
set.seed(1)
flight.tree = rpart(wasdelayed ~ Flight_Distance + precipIntensity + precipProbability + temperature + apparentTemperature + dewPoint + humidity + pressure + windSpeed + windGust + windBearing + cloudCover + uvIndex + visibility + ozone + precipType + precipAccumulation,
                    data = flightinfo, method = "class", control = rpart.control(cp = 0.005))
pdf("flighttree.pdf", width=8, height=8)
plot(flight.tree)
text(flight.tree)
dev.off()
# Interesting that ozone is the best split; it's unclear why that would be
# Makes sense that greater cloud cover leads to more delays (shows up in several places)

flightinfo.rel = flightinfo[, c(34, 5, 18:33)]

# Begin modeling with logistic regression
flight.lr = glm(wasdelayed ~ ., data = flightinfo.rel, family = "binomial")
summary(flight.lr)
# Flight distance isn't particularly helpful, so we should pretty much
# only look at weather
# Several aspects of weather are important. It seems that snowy weather
# is more likely to delay a plane than rainy
# Also interestingly, here ozone isn't very important in predicting
sum((flight.lr$fitted.values >= 0.5) == flightinfo$wasdelayed) / nrow(flightinfo)
# Training accuracy is around 78%

# Let's try a random forest
set.seed(1)
flight.rf = randomForest(wasdelayed ~ ., data = flightinfo.rel, ntree = 500)
ntree.best = which.min(flight.rf$err.rate[, "OOB"])
flight.rf.opt = randomForest(wasdelayed ~ ., data = flightinfo.rel, ntree = ntree.best)
sum(flight.rf.opt$predicted == flightinfo$wasdelayed) / nrow(flightinfo)
# Again, training accuracy is around 78%
# A random forest does not perform much better than logistic regression

# Let's try LDA and QDA
flight.lda = lda(wasdelayed ~ ., data = flightinfo.rel)
sum(predict(flight.lda, flightinfo.rel)$class == flightinfo.rel$wasdelayed) / nrow(flightinfo.rel)
# Again, training accuracy is around 78%
flight.qda = qda(wasdelayed ~ ., data = flightinfo.rel)
sum(predict(flight.qda, flightinfo.rel)$class == flightinfo.rel$wasdelayed) / nrow(flightinfo.rel)
# Training accuracy is lower at 77%

# Assess cross-validated accuracy of each method
ffcv = function(method, folds)
{
  err = NULL
  for (i in 1:5)
  {
    if (method == "glm")
    {
      mod = glm(wasdelayed ~ ., data = flightinfo.rel[folds != i,], family = "binomial")
      pred = predict(mod, flightinfo.rel[folds == i,]) >= 0.5
    }
    else if (method == "rf")
    {
      mod = randomForest(wasdelayed ~ ., data = flightinfo.rel[folds != i,], ntree = ntree.best)
      pred = predict(mod, flightinfo.rel[folds == i,])
    }
    else if (method == "lda")
    {
      mod = lda(wasdelayed ~ ., data = flightinfo.rel[folds != i,])
      pred = predict(mod, flightinfo.rel[folds == i,])$class
    }
    else if (method == "qda")
    {
      mod = qda(wasdelayed ~ ., data = flightinfo.rel[folds != i,])
      pred = predict(mod, flightinfo.rel[folds == i,])$class
    }
    err = c(err, sum(pred != flightinfo.rel[folds == i, 1]) / length(pred))
  }
  return(mean(err))
}
set.seed(1)
folds = sample(1:5, nrow(flightinfo), replace = TRUE)
ffcv("glm", folds)
ffcv("rf", folds)
ffcv("lda", folds)
ffcv("qda", folds)

# The lowest cross-validated error comes from RF, so this is chosen as
# the final model
























