##########################################1Q######################################
library(zoo)
library(tsfeatures)

'Read timeseries .rda data '
ts_data <- readRDS("pneu_flu.rda")

'What is the start and end points in this timeseries.'

start_date <- start(ts_data)
end_date <- end(ts_data)
cat("Start Date:", start_date, "\n")
cat("End Date:", end_date, "\n")

'How many time series observations are in this dataset?'
num_observations <- length(ts_data)
cat("Number of Observations:", num_observations, "\n")

'Is there any trend and seasonality? If yes then, what are i.Frequency ii.Number of periods iii.Seasonal period iv.Trend constant value'

ts_features <- tsfeatures(ts_data)
frequency <- ts_features$frequency
num_periods <- length(ts_data) / frequency
seasonal_period <- ts_features$seasonal_period
trend <- ts_features$trend

cat("Frequency:", frequency, "\n")
cat("Number of Periods:", num_periods, "\n")
cat("Seasonal Period:", seasonal_period, "\n")
cat("Trend Constant Value:", trend, "\n")

'd.What the decompose timeseries tells you. How the decomposition information is compared with extracted features.'
decomposed <- decompose(ts_data)
print(decomposed)
plot(decomposed)
'The frequency and number of periods from the  extracted features exactly match the seasonal component in the  decomposition.'

'e Partition dataset into train and validation. Make a reasonable choice for partitions’ size'
nValid <- 36
nTrain <- length(ts_data) - nValid
train.ts <- window(ts_data, start = c(1968, 1), end = c(1968, nTrain))
valid.ts <- window(ts_data, start = c(1968, nTrain + 1), end = c(1968, nTrain + nValid))

'f.Use Holts-Winter algorithm to build a forecasting model. Apply the model on validation.  Use these constants for Holts-Winter algorithm 
alpha = 0.2, beta = 0.1, gamma = 0.1'
# Build Holt-Winters model
hw_model <- HoltWinters(train.ts, alpha = 0.2, beta = 0.1, gamma = 0.1)
hw_forecast <- forecast(hw_model, h = length(valid.ts))
forecast_values <- hw_forecast$mean

'g.Plot training, actual validation, forecast validation, residuals, and errors in one plot. Interpret the result through your visualizations'
# Plot training, actual validation, forecast validation
plot(ts_data, col = "blue", ylim = c(min(ts_data, forecast_values), max(ts_data, forecast_values)), main = "Time Series Forecasting")
lines(train.ts, col = "green")
lines(valid.ts, col = "pink")
lines(valid.ts-forecast_values, col = "red")
lines(forecast_values, col = "orange")
lines(residuals(hw_model) , col = "purple")
legend("topright", legend = c("Time Series Data", "Training", "Validation", 
                              "Error", "Forecasted Values","Residual"), 
       col = c("blue", "green", "pink", "red", "orange","purple"), lty = 1, cex = 0.7)

'h Forecast for the next six months'
future_forecast <- forecast(valid.ts, h = 6)
print(future_forecast)

'i.Plot the dataset and the 3 years forecast in one plot. What can you say for your forecast errors?'
future_forecast <- forecast(valid.ts, h = 36)
print(future_forecast)
plot(ts_data, col = "blue", ylim = c(min(ts_data, future_forecast$mean), max(ts_data, future_forecast$mean)), 
     main = "Dataset with 3-Year Forecast", xlab = "Date", ylab = "Value")
lines(future_forecast$mean, col = "orange")
legend("topright", legend = c("Dataset", "3-Year Forecast"), col = c("blue", "orange"), lty = 1, cex = 0.7)


#####################################2Q###########################################


library(forecast)
'a Built timeseries of this data'
air_passengers <- read.csv("AirPassengers.csv", header = TRUE)
ts_air_passengers <- ts(air_passengers$X.Passengers, start = c(1949, 1), frequency = 12)
plot(ts_air_passengers , main = "DATASET")

'b Decompose the time series to get its components'
decomposed_air_passengers <- decompose(ts_air_passengers)
decomposed_air_passengers$trend
decomposed_air_passengers$seasonal
decomposed_air_passengers$random
plot(decomposed_air_passengers)

'c.Plot AirPassengers  timeseries with trend. Make sure your trend is accurately identified.'
plot(ts_air_passengers, main = "Air Passengers Time Series with Trend", xlab = "Year", ylab = "Passengers", col = "black")
lines(decomposed_air_passengers$trend, col = "blue")

'd.Partition data in 24 month validation and rest for training'
nValid <- 24
nTrain <- length(ts_air_passengers) - nValid
train.ts <- window(ts_air_passengers, start = c(1949, 1), end = c(1949, nTrain))
valid.ts <- window(ts_air_passengers, start = c(1949, nTrain + 1), end = c(1949, nTrain + nValid))

'e.Build naïve and seasonal naïve models to forecast validation dataset, get the accuracy of these models. '
naive.pred <- naive(train.ts, h=nValid)
naive_accuracy = accuracy(naive.pred , valid.ts)
snaive.pred <- snaive(train.ts, h=nValid)
seasonal_naive_accuracy = accuracy(snaive.pred , valid.ts)
naive_accuracy
seasonal_naive_accuracy
'f.Use the  appropriate model setting for Exponential Smoothing function ets() to build a forecast model. Apply the model to forecast validation time series.'
ets_model <- ets(train.ts, model = "MAM")
ets_forecast <- forecast(ets_model, h = nValid)
ets_accuracy <- accuracy(ets_forecast, valid.ts)
ets_accuracy
'g.Get the accuracy forecast and compare the RMSE, MAE, and MAPE of this model with your benchmark model'
benchmark_models <- data.frame(Model = c("Naïve", "Seasonal Naïve", "ETS"),
                               RMSE = c(naive_accuracy[2], seasonal_naive_accuracy[2], ets_accuracy[2]),
                               MAE = c(naive_accuracy[3], seasonal_naive_accuracy[3], ets_accuracy[3]),
                               MAPE = c(naive_accuracy[5], seasonal_naive_accuracy[5], ets_accuracy[5]))
print(benchmark_models)

'h.Put are models’ forecast and timeseries in the same plot'
plot(ts_air_passengers, col = "blue", ylim = c(min(ts_air_passengers, ets_forecast$mean), max(ts_air_passengers, ets_forecast$mean)), main = "Air Passengers Time Series and Forecast")
lines(ets_forecast$mean,  col = "darkred")
lines(naive.pred$mean,col="green")
lines(snaive.pred$mean, col="purple")

legend("topleft", legend = c("Air Passengers", "ETS Forecast", "Naive Forecast", "Seasonal Naive Forecast"), 
       col = c("blue", "darkred", "green", "purple"), lty = c(1, 1, 1, 1), lwd = c(1, 1, 2, 2))