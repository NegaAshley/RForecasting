# Ashley Xu
# CS 492
# Forecasting Assignment
# S&P 500 Dataset 
#(https://www.kaggle.com/jes2ica/sp-500/version/1#_=_)
# Births in U.S. 1994 to 2003 
# (https://www.kaggle.com/adnanr94/births-in-us-1994-to-2003)

# Libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggplot)
library(forecast)
library(fpp2)
library(fpp)
library(lubridate)

# Read the data from Excel into R
sp500 <- read_csv("sp500.csv")
births <- read_csv("births.csv")

# Make into time series
tssp500 <- ts(sp500)
tsbirths <- ts(births)

# Plot the data with facetting
autoplot(tssp500, facets = TRUE)
# Trend - increase
# Seasonal - unsure
# Cyclical - maybe in the micro, but hard to see at a macro level
autoplot(tsbirths, facets = TRUE)
# Trend - increase
# Seasonal -looks seasonal
# Cyclical - looks cyclical

# Cutting down to just column to keep track of
sp500 <- sp500[ , -c(3:7)]
# Get rid of day of week
births <- births[ , -c(4)]

# Fix dates in births
# Create character string formatted as "YYYY-MM-DD"
# Use Lubridate ymd to make into vector of dates
form.dates <- ymd(paste(births$year, births$month, births$date_of_month, sep = "-"))

# Create a new column in births for formatted dates
births$date <- form.dates

# Get rid of the unneeded columns
births <- births[, -c(1:3)]

# Make into time series
tssp500 <- ts(sp500[ , 2], frequency = 365.25)
tsbirths <- ts(births[ , 1], frequency = 365.25)


# Plot the data
autoplot(tssp500)
autoplot(tsbirths)

# Create the training data
# First param is dataset, second is number of observations
trainsp500 <- subset(tssp500, start = 0, end = length(sp500) - 1000)
trainbirths <- subset(tsbirths, start = 0, end = length(births) - 1000)

# Naive datasets
naivesp500 <- naive(trainsp500, h = 1000)
naivebirths <- naive(trainbirths, h = 1000)

# Ses datasets
sessp500 <- ses(trainsp500, h = 1000)
sesbirths <- ses(trainbirths, h = 1000)

# Test accuracy of SP500 forecasts
accnaivesp500 <- accuracy(naivesp500, tssp500)
accsessp500 <- accuracy(sessp500, tssp500)

accnaivesp500
# Test RMSE is 522.82
# TEST MAPE is 22.89
accsessp500
# Test RMSE is 521.03 - lower, which is better
# Test MAPE is 22.81 - lower, which is better

# Check the residuals
naivesp500 %>% checkresiduals()
# Follows a normal distrubtion, which is good
# Some points of ACF fall outside of blue line,
# which may mean it's not white noise
# P-value is 0.001119, which is less than .05
# This means the lag is white noise

sessp500 %>% checkresiduals()
# FOllows a normal distribution, which is good
# Some points of ACF fall outside of blue line,
# which may mean it's not white noise
# P-value is 0.001763, which is less than .05
# This means the lag is white noise

# In the case of Naive vs SES on the SP500 data,
# the SES forecast is better.

# Test accuracy of SP500 forecasts
accnaivebirths <- accuracy(naivebirths, tsbirths)
accsesbirths <- accuracy(sesbirths, tsbirths)

accnaivebirths
# Test RMSE is 2471.29
# Test MAPE is 19.63

accsesbirths
# Test RMSE is 2122.99
# Test MAPE is 18.98
# Both of these are lower, which is better

# Check the residuals
naivebirths %>% checkresiduals()
# The ACF values are way outside of the blue
# line, which indicates that there are external
# variables that need to be accounted for
# Distribution is mostly normal

sesbirths %>% checkresiduals()
# The ACF values are way outside of the blue
# line, which indicates that there are external
# variables that need to be accounted for
# The distribution is not normal

# SES is a little better for births, but not much

# Doing ETS
fitsp500 <- ets(tssp500)
fitbirths <- ets(tsbirths)

# Check residuals
checkresiduals(fitsp500)
# Passes test of white noise

# Plot forecast
# Giving model, not forecast itself
fitsp500 %>% forecast() %>% autoplot()
# Additive errors
# No trend
# No seasonality - isssue may be that there are too
# Many points in data
# The model fits better for this

# Repeat for births
checkresiduals(fitbirths)
# Does not pass white noise - problem

fitbirths %>% forecast() %>% autoplot
# Multiplicative error
# Dampened trend
# No seasonality - could be issue
# with the way that I created the 
# timeseries
# The model is not good for this