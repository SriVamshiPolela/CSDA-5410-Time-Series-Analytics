#Use air passengers count in “AirPassenger.csv” file to:
AirPassengers = read.csv("AirPassengers.csv")
print(AirPassengers)
#a.	Built timeseries of this data
AirPassengers_ts = ts(AirPassengers$X.Passengers, start = c(1949,1), end = c(1960,12), frequency = 12)
plot(AirPassengers_ts, xlab= "Time", ylab="Air Passengers", main = "Air Passengers from 1949 - 1960")

#b.	Get the AirPassengers timeseries components

DecomposedAirPassengers = decompose(AirPassengers_ts)
DecomposedAirPassengers$trend
DecomposedAirPassengers$seasonal
DecomposedAirPassengers$random
DecomposedAirPassengers$type # it says time series is additive but from seasonal plot it seems to be multiplicative. 

#c.	Plot AirPassengers  timeseries with trend. Make sure your trend is accurately identified.
plot(AirPassengers_ts, main = "AirPassengers Time Series with Trend", ylab = "Passenger Count")
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

# 
# 

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