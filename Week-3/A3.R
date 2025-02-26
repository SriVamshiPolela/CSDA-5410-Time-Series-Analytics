
# _______________________________________Assignment -3________________________________________

# ______________________________________Question 1____________________________________________

# Load required libraries
library(zoo)

# a.	Read timesereis .rda data 
PIDeaths = readRDS("pneu_flu.rda")
print(PIDeaths)
plot(BoxCox(PIDeaths, 0))


# b.	Install package “tsfeatures” and activate it, then you can use the tsfeatures() to extract your timeseries dataset features. 
# options(repos = "https://cran.r-project.org")
# install.packages("tsfeatures")

library(tsfeatures)

# c.	What is the start and end points in this timeseries. 
start(PIDeaths)
end(PIDeaths)
# How many time series observations are in this dataset?
length(PIDeaths)

'Is there any trend and seasonality? If yes then, what are:
i.  Frequency
ii.	Number of periods
iii.Seasonal period
iv.	Trend constant value'

# Using tsfeatures we can extract the trend and seasonality information

# Analyze the time series data for trend and seasonality
features = tsfeatures(PIDeaths)
str(features)
# Yes there is trend and seasonality.

# Extract seasonality information
features$frequency  # 12
features$nperiods  # 1
features$seasonal_period # 12
# Extract trend information
features$trend  # 0.2904842


# Plot the time series data
plot(PIDeaths, main = "Monthly Pneumonia and Influenza Deaths per 10,000")
# Add trend line using linear regression

abline(lm(PIDeaths ~ time(PIDeaths)), col = "blue")


# d.	What the decompose timeseries tells you. 
# How the decomposition information is compared with extracted features.

'The seasonal characteristics we identified in the data match the decomposition results, Confirming the yearly cyclic pattern. 
 This alignment validates our understanding of the seasonal behavior within the dataset, enhancing our confidence in its analysis and forecasting.'


# Decompose time series data
decomposed = decompose(PIDeaths) 

# Plot decomposed components
plot(decomposed)

# Extract decomposed components
decomposed$trend
decomposed$seasonal
decomposed$random

# e. Partition dataset into train and validation. Make a reasonable choice for partitions’ size
# Data partitioning
nValid <- 36  # there are total 132 periods means (11 years) from that 96 periods means(8 years) goes to train and 36 periods means (3 years) for validation
length(PIDeaths)
ntrain <- length(PIDeaths) - nValid
train.ts <- window(PIDeaths, start = c(1968, 1), end = c(1968, ntrain))
valid.ts <- window(PIDeaths, start = c(1968, ntrain + 1), end = c(1968, ntrain + nValid))
train.ts
valid.ts

library(forecast)
# f.	Use Holts-Winter algorithm to build a forecasting model. Apply the model on validation.  
# Use these constants for Holts-Winter algorithm  alpha = 0.2, beta = 0.1, gamma = 0.1

HW_PIDeaths = HoltWinters(train.ts, alpha = 0.2, beta = 0.1, gamma = 0.1)
HW_PIDeaths_forecast = forecast(HW_PIDeaths, h = nValid)

# g.	Plot training, actual validation, forecast validation, residuals, and errors in one plot. 
# Interpret the result through your visualizations

plot(train.ts, col = "blue", main = "Training, Actual, and Forecasted Time Series",
     ylim = c(-0.4, 0.8), xlim = c(1968,1978),
     xlab = "Year", ylab = "Number of Deaths")
lines(valid.ts, col = "green", lty = 2)  # Actual validation data
lines(HW_PIDeaths_forecast$mean, col = "red")  # Forecasted data
lines(valid.ts - HW_PIDeaths_forecast$mean, col = "darkred")
lines(HW_PIDeaths_forecast$residuals , col = "purple")
# Add legend
legend("topright", legend = c("Training", "Actual Validation", "Forecasted", "Error", "Residual"), 
       col = c("blue", "green", "red", "darkred","purple"), lty = c(1, 2, 1, 1, 1), bty = "n")




# h.	Forecast for the next six month in the future.
future_forecast = forecast(HW_PIDeaths, h = )
print(future_forecast)

# i.	Plot the dataset and the 3 years forecast in one plot. What can you say for your forecast errors?
Three_years_forecast = forecast(valid.ts, h = 36)

plot(PIDeaths, xlab = "Year", ylab = "Number of Deaths", xlim = c(1968, 1982), ylim = c(0.2,0.8), main = "Dataset with three years forecasted")
lines(Three_years_forecast$mean, col = "green")

# Add legend
legend("topright", legend = c("Actual Data", "Forecasted 3 years"), 
       col = c("black","green"), lty = c(1, 1), bty = "1")



# ____________________________________________________________________________________________
# ______________________________________Question 2____________________________________________

#Use air passengers count in “AirPassenger.csv” file to:
AirPassengers = read.csv("AirPassengers.csv")
print(AirPassengers)

#a.	Built timeseries of this data
AirPassengers_ts = ts(AirPassengers$X.Passengers, start = c(1949,1), end = c(1960,12), frequency = 12)
plot(AirPassengers_ts, xlab= "Time", ylab="Air Passengers", main = "Air Passengers from 1949 - 1960")

# Here  
'AirPassengers_ts_1 = ts(AirPassengers$X.Passengers*100, start = c(1949,1), end = c(1960,12), frequency = 12)
plot(AirPassengers_ts_1, xlab= "Time", ylab="Air Passengers", main = "Air Passengers from 1949 - 1960")
'
#b.	Get the AirPassengers timeseries components

DecomposedAirPassengers = decompose(AirPassengers_ts)
DecomposedAirPassengers$trend
DecomposedAirPassengers$seasonal
DecomposedAirPassengers$random
DecomposedAirPassengers$type # it says time series is additive but from seasonal plot it seems to be multiplicative. 

#c.	Plot AirPassengers  timeseries with trend. Make sure your trend is accurately identified.
plot(AirPassengers_ts, main = "AirPassengers Time Series with Trend", ylab = "Passenger Count")
# Add trend line
lines(DecomposedAirPassengers$trend, col = "red")

#d.	Partition data in 24 month validation and rest for training
# Data partitioning
nValid <- 24   
length(AirPassengers_ts)
ntrain <- length(AirPassengers_ts) - nValid
train.ts <- window(AirPassengers_ts, start = c(1949, 1), end = c(1949, ntrain))
valid.ts <- window(AirPassengers_ts, start = c(1949, ntrain + 1), end = c(1949, ntrain + nValid))

train.ts
valid.ts

# e.	Build naïve and seasonal naïve models to forecast validation dataset, get the accuracy of these models. 
library(forecast)
# Naive Model
naive.pred = naive(train.ts, h = nValid)
accuracy(naive.pred, valid.ts)
forecast(naive.pred, h=nValid)
# Seasonal Naive Model
snaive.pred = snaive(train.ts, h=nValid)
accuracy(snaive.pred, valid.ts)


# f.	Use the  appropriate model setting for Exponential Smoothing function ets() to build a forecast model. 
# Apply the model to forecast validation time series.

ets.Model = ets(train.ts, model = "MAM") 

# ets.Model = ets(train.ts) # ets function automatically select which model is suitable for this time series data
# plot(ets.Forecast)

ets.Forecast = forecast(ets.Model, h =nValid)
accuracy(ets.Forecast,valid.ts)
print(ets.Forecast)


#g.	Get the accuracy forecast and compare the RMSE, MAE, and MAPE of this model with your benchmark model

# Benchmark Models Accuracy
naive_accuracy <- accuracy(naive.pred, valid.ts)
snaive_accuracy <- accuracy(snaive.pred, valid.ts)

# ETS Model Accuracy
ets_accuracy <- accuracy(ets.Forecast, valid.ts)

# Comparison of three models with their RMSE, MAE, MAPE
comparison <- data.frame(
  Model = c("Naive", "Seasonal Naive", "ETS"),
  RMSE = c(naive_accuracy[2], snaive_accuracy[2], ets_accuracy[2]),
  MAE = c(naive_accuracy[3], snaive_accuracy[3], ets_accuracy[3]),
  MAPE = c(naive_accuracy[5], snaive_accuracy[5], ets_accuracy[5])
)

print(comparison)

#h.	Put are models’ forecast and timeseries in the same plot

# Plotting original time series data

plot(AirPassengers_ts, main = "Comparison of Forecast Models", xlab = "Time", ylab = "Passenger Count")
lines(valid.ts, col = "blue", lty =1)# Plotting validation dataset

# Adding forecasts for each model
lines(naive.pred$mean, col = "red")  # Naive forecast
lines(snaive.pred$mean, col = "green")  # Seasonal naive forecast
lines(ets.Forecast$mean, col = "purple", lty = 4)  # ETS forecast

# Adding legend
legend("topleft", legend = c("Validation", "Naive", "Seasonal Naive", "ETS"), 
       col = c("blue", "red", "green", "purple"), lty = c(1, 1, 1, 4), bty = "n")


# ets() model gives a better performance when compared to RMSE, MAE, MAPE
