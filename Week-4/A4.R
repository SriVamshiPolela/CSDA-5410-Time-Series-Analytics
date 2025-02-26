# _______________________________________Assignment-4________________________________________

# ______________________________________Question 1____________________________________________

# Load required libraries
library(zoo)
library(forecast)

# a.	Read timesereis .rda data 
PIDeaths = readRDS("pneu_flu.rda")
print(PIDeaths)

# a.	Partition dataset into train and validation. Make a reasonable choice for partitions’ size.
# Data partitioning
nValid <- 36  # there are total 132 periods means (11 years) from that 96 periods means(8 years) goes to train and 36 periods means (3 years) for validation
length(PIDeaths)
ntrain <- length(PIDeaths) - nValid
train.ts <- window(PIDeaths, start = c(1968, 1), end = c(1968, ntrain))
valid.ts <- window(PIDeaths, start = c(1968, ntrain + 1), end = c(1968, ntrain + nValid))
train.ts
valid.ts
plot(train.ts)
plot(valid.ts)

# b.	Use the regression model to fit a forecasting model. Run the model on validation.  
Regression_model = tslm(train.ts ~ trend + season, lambda = 1)

summary(Regression_model)
forecast_valid = forecast(Regression_model, h= nValid, level = 0)
head(forecast_valid)
# d.	Put training, time series trend, actual validation, forecasted validation on the same plot. Interpret the result through your visualizations.
# Extract time series trend
decomposed <- decompose(train.ts)
trend <- decomposed$trend

plot(train.ts, main = "Training, time series trend, actual validation, forecasted validation ", xlab = "Time", ylab = "Death Count", xlim = c(1968, 1980), ylim = c(0.1,0.8))
lines(trend, col = "orange")
lines(valid.ts, col = "blue", lty =1)# Plotting validation dataset
lines(forecast_valid$mean, col = "red")
legend("topright", legend = c("Training", "Time Series Trend", "Actual Validation", "Forecasted Validation"), 
       col = c("black", "orange", "blue", "red"), lty = c(1, 1, 1, 1), cex = 0.8)
# e.	Get the performance of this model.
accuracy(forecast_valid$mean, valid.ts)
# f.	Forecast for the next 4 month in the future.
Model= tslm(PIDeaths ~ trend + season, lambda = 1)
summary(Model)
forecast_4 = forecast(Model, h=4)

# g.	Plot the dataset and the 3 months forecast in one plot. What can you say for your forecast errors?
forecast_3 = forecast(Model,h=3)

plot(PIDeaths, main = "Actual Dataset with 3 months forecast", xlab = "Time", ylab = "Death Count", xlim = c(1968, 1980), ylim = c(0.1,0.8))
lines(forecast_3$mean, col = "blue", lty =1)
legend("topright", legend = c("Actual Data", "Forecasted Future"), 
       col = c("black", "blue"), lty = c(1, 1), cex = 0.8)




#_________________________________________________________________________________________________
# Question -2

#Use air passengers count in “AirPassenger.csv” file to:
AirPassengers = read.csv("AirPassengers.csv")
print(AirPassengers)
#a.	Built timeseries of this data with
AirPassengers_ts = ts(AirPassengers$X.Passengers, start = c(1949,1), end = c(1960,12), frequency = 12)
# b.	Put the timeseries and its level on the same plot. 
plot(AirPassengers_ts, xlab= "Time", ylab="Air Passengers", main = "Air Passengers from 1949 - 1960")
abline(reg = lm(AirPassengers_ts ~ time(AirPassengers_ts)),col="blue")
lines(decompose(AirPassengers_ts)$trend, col = "red")
# Add legend
legend("topleft", legend = c("Time series data", "Level", "Trend"),
       col = c("black", "blue", "red"), lty = 1)

#d.	Partition data in 24 month validation and rest for training
# Data partitioning
nValid <- 24   
length(AirPassengers_ts)
ntrain <- length(AirPassengers_ts) - nValid
train.ts <- window(AirPassengers_ts, start = c(1949, 1), end = c(1949, ntrain))
valid.ts <- window(AirPassengers_ts, start = c(1949, ntrain + 1), end = c(1949, ntrain + nValid))

train.ts
valid.ts

# e.	Fit the regression model on the training to build a forecasting model (model A). 
Model_A = tslm(train.ts ~ trend + season, lambda = 1)
summary(Model_A)

# f. Run Model A on validation and get the RMSE, MPE, and MAPE
forecast_A = forecast(Model_A, h = 24)
accuracy_A = accuracy(forecast_A, valid.ts)
accuracy_A


# g.	Use the appropriate model setting for Exponential Smoothing function ets() to build a forecast model (model B). 
Model_B = ets(train.ts)
plot(Model_B)
summary(Model_B)

# h.	Run the model (model B) on validation and get the RMSE, MPE, and MAPE
forecast_B = forecast(Model_B, h = 24)
accuracy_B = accuracy(forecast_B, valid.ts)
accuracy_B

# i.	Get the accuracy forecast and compare the RMSE, MAE, and MAPE of these models with your benchmark model. Which one is a better model
benchmark = snaive(train.ts)
accuracy_benchmark = accuracy(benchmark, valid.ts)

# Compare the accuracy measures of the models
accuracy_A
accuracy_B
accuracy_benchmark




# j.	Put training, validation and the validation forecasts of A and B models on the same plot
plot(train.ts, col = "black", ylim = c(0, 600), xlim = c(1949,1960),
     xlab = "Time", ylab = "Air Passengers", main = "Training, Validation, and Forecasted Values")

# Adding validation data
lines(valid.ts, col = "blue")

# Adding forecasted values from Model A
lines(forecast_A$mean, col = "red")

# Adding forecasted values from Model B
lines(forecast_B$mean, col = "green")

# Adding legend
legend("topleft", legend = c("Training", "Validation", "Forecast A", "Forecast B"), 
       col = c("black", "blue", "red", "green"), lty = c(1, 1, 1, 1))



#k.	Use model A to manually calculate the air passenger count for November and December of 1961. Do not use R. 
Model_A_totaldata = tslm(AirPassengers_ts ~ trend + season, lambda = 1)
summary(Model_A_totaldata)




