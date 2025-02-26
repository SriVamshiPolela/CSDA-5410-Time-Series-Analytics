#________________________________Assignment_6_____________________________________
# Monthly Carbon Dioxide Levels at Mauna Loa (50 Points)

# libraries required
library(tsfeatures)
library(lubridate)
library(forecast)

# 1.	Read the co2.df
co2.df = read.csv("cardox.csv")
head(co2.df)
tail(co2.df)

# 2.	Prepare proper timeseries dataset 
'we can also use msts() to convert df to timeseries which can also allow us to give two seasons to it like multi seasonality example; day =365, frequ =12' 
co2.ts = ts(co2.df$cardox, start = c(1958,3), end = c(2023, 3), frequency = 12)
plot(co2.ts, main = "Carbon Dioxide Levels at Mauna Loa")

# 3.	What month in all years has the highest level of CO2?
seasonplot(co2.ts, main = "Seasonal Plot of Carbon Dioxide Levels at Mauna Loa")
splot  = window(co2.ts, start = c(1958,3), end = c(1963,3))
seasonplot(splot, main = "Seasonal Plot for first five years of Carbon Dioxide Levels at Mauna Loa")


'A.	Timeseries exploration
a.	Use the decompose() to Extract the residuals, trend, and seasonality values from timeseries data'

co2_decompose = decompose(co2.ts)
co2_decompose$random
co2_decompose$seasonal
co2_decompose$trend
plot(co2_decompose)
'
regression_Modell = tslm(co2.ts~trend + season)
regression_Modell$residuals + regression_Modell$rank
'

# Rebuild the dataset by summing the components
rebuilt_data = co2_decompose$random + co2_decompose$seasonal + co2_decompose$trend

plot(co2.ts, main = " Rebuilt data with original data")
lines(rebuilt_data, col = "green", lty = 2)
legend("topleft", legend = c("Original data", "Rebuilt data"), col = c("black", "green"), lty = c(1,2), bty = "n")


# B.	Building Models
# a.	Partition timeseries
# b.	Training from 1956 March to 2019 March
# c.	Validation 2019 March to 2023 March
nvalid = 48
ntrain = length(co2.ts) - nvalid
train.ts = window(co2.ts, start = c(1958,3), end = c(2019,3))
valid.ts = window(co2.ts, start = c(2019, 4), end = c(2023, 3))
head(train.ts)
length(train.ts)
head(valid.ts)
length(valid.ts)
tail(train.ts)

#d.	Build three type of regression models
# •	Linear regression
Linear_Regression = tslm(train.ts~trend + season)

# •	Parabolic regression
Parabolic_regression = tslm(train.ts~trend + I(trend^2) + season)

# •	Exponential regression
Exponential_regression = tslm(train.ts~trend+season, lambda = 0)

# e.	Build triple exponential smoothing with ets() use an MAA setting
tes = ets(train.ts,model = "MAA")

# C.	Forecast validation with these models and compare their performance measures.
# Forescasting with Linear regression model
LM_forecast = forecast(Linear_Regression, h = nvalid)
accuracy(LM_forecast, valid.ts)
# Forescasting with Parabolic regression model
PB_forecast = forecast(Parabolic_regression,  h = nvalid)
accuracy(PB_forecast,valid.ts)
# Forescasting with Exponential regression model
EX_forecast = forecast(Exponential_regression, h= nvalid)
accuracy(EX_forecast, valid.ts)
# Forescasting with Triple exponential smoothing with ets() 
tes_forecast = forecast(tes, h= nvalid)
accuracy(tes_forecast, valid.ts)

# D.	Prepare required plots. DO not generate irrelevant plots. 
plot(train.ts, xlab = "time", ylab = "co2 level", xlim = c(1956,2023), ylim =c(300,450), main = "Time Series forecasted using four models")
lines(valid.ts, col = "blue")
lines(LM_forecast$mean, col = "red")
lines(PB_forecast$mean, col = "green")
lines(EX_forecast$mean, col = "orange")
lines(tes_forecast$mean, col = "yellow")
legend("topleft", legend = c("Training", "Validation", "Linear", "Parabolic", "Exponential", "Triple Exponential"), 
       col = c("black","blue","red","green","orange","yellow"),lty =c(1,1,2,2,2,2), bty = "n")


# F.	Manually forecast for December 2023 and January 2024 with your best regression model.
Parabolic_regression1 = tslm(co2.ts~trend + I(trend^2) + season)
# Set options to display numeric values without scientific notation
options(scipen = 999)
summary(Parabolic_regression1)
summary(Parabolic_regression1)$coefficients[, "Estimate"]
forecast(Parabolic_regression1, h = 10) 

#_______________________________________________Question_2_____________________________________________
# 1.	Read the hor.df
hor.df = read.csv("hor.csv")
head(hor.df)
tail(hor.df)
# dataset has quarterly 
hor.ts = ts(hor.df$hor, start = c(1982,1), end = c(2015,4), frequency = 4)
# A.	Timeseries exploration
# a.	Prepare the decompose plot of timeseries. Interpret timeseries components
hor_decompose = decompose(hor.ts)
plot(hor_decompose)

# b.	Plot autocorrelation and partial autocorrelation for 12, 36, and all periods.
# ACF and PACF for lag period of 12
par(mfrow=c(3,2)) 
acf(hor.ts, lag.max = 12, main = "ACF for Lag Period of 12")
pacf(hor.ts, lag.max = 12, main = "PACF for Lag Period of 12")
#par(mfrow=c(2,1)) 
# ACF and PACF for lag period of 36
acf(hor.ts, lag.max = 36, main = "ACF for Lag Period of 36")
pacf(hor.ts, lag.max = 36, main = "PACF for Lag Period of 36")
#par(mfrow=c(2,1)) 
# ACF and PACF for all periods
acf(hor.ts, main = "ACF for All Periods")
pacf(hor.ts, main = "PACF for All Periods")
par(mfrow=c(1,1)) 

# c.	What do you conclude of these plots for your further work on this case.

# B.	Building models
# a.	Partition timeseries
length(hor.ts)
# b.	Training from 1982 January to 2009 December
train.ts = window(hor.ts, start = c(1982,1), end = c(2009,4))
# c.	Validation 2010 January to 2015 December
valid.ts = window(hor.ts, start = c(2010, 1), end = c(2015, 4))

# d.	Build three type of regression models
# •	Linear regression with season
Linear_Regression = tslm(train.ts~ season)


# •	Build an ets() model with proper model setting. 
ets_model = ets(train.ts, model = "MNA")
plot(ets_model)

# •	Build an autoregressive model with residuals
ar_model = arima(ets_model$residuals, order = c(2,0,0))

# E.	Forecast validation with ets() models and AR(). 
ets_forecast = forecast(ets_model, h = length(valid.ts))
ar_forecast = forecast(ar_model, h= length(valid.ts))
ets_forecast
ar_forecast
# H.	Forecast for three years (2016-2019) into the future using a model built with ets()
ets_model_1 = ets(hor.ts, model = "MNA")
ets_forecast_future = forecast(ets_model_1, h = 16)
ets_forecast_future

# I.	Prepare a plot of 
# a.	Training timeseries
# b.	Validation timeseries 
# c.	The ets model forecast.
# d.	The AR forecast 
# e.	Sum of ets and AR forecast values
plot(train.ts, xlab = "Time", ylab = "Hotel Occupancy", xlim = c(1982, 2020), 
     ylim = c(0,100), main = "Forecasting Occupancy")
lines(valid.ts, col = "blue", lty = 2)
lines(ets_forecast$mean, col = "yellow", lty = 1)
lines(ar_forecast$mean, col = "green", lty = 1)
lines(ets_forecast$mean + ar_forecast$mean, col = "purple", lty = 2)
legend("bottomleft", legend = c("Training", "Validation", "ETS Forecast", "AR Forecast", "Sum of ETS and AR"), col = c("black", "blue", "yellow", "green", "purple"), lty = c(1, 2, 1, 1, 2))

# J.	Prepare a plot of the future forecast
plot(train.ts, xlab = "Time", ylab = "Hotel occupancy", xlim = c(1982, 2020), 
     ylim = c(55,100), main = "Future Forecasting Occupancy ")
lines(valid.ts, col = "blue", lty = 2)
lines(ets_forecast_future$mean, col = "red", lty = 2)
legend("topleft", legend = c("Original data","Validation","ETS future Forecast"), col = c("black", "blue", "red"), lty = c(1, 2, 2))



#_______________________________________________Question_3____________________________________________
# 1.	(25 Points) Do all parts of problem 3 on page 172. 
# a.	Run a regression model with log (Sales) as the output variable and with a linear trend and monthly predictors. 
# Use this model to forecast the sales in January 2001, January 2002, and February 2002. 
# Think carefully which data to use for model fitting in each case.


library(forecast)
SouvenirSales.df = read.csv("SouvenirSales.csv") 
head(SouvenirSales.df)
tail(SouvenirSales.df)
SouvenirSales.ts = ts(SouvenirSales.df$Sales, start = c(1995,1), end = c(2001,12), frequency = 12)
plot(SouvenirSales.ts, main = "Monthly SouvenirSales")
plot(log(SouvenirSales.ts))
acf(SouvenirSales.ts, lag.max = 12, main="ACF plot")
length(SouvenirSales.ts)

# Data partitioning 
nvalid = 12
ntrain = length(SouvenirSales.ts) - nvalid
train.ts = window(SouvenirSales.ts, start = c(1995,1), end = c(1995,ntrain))
valid.ts = window(SouvenirSales.ts, start = c(1995,ntrain + 1), end = c(1995, ntrain + nvalid))
head(train.ts)
length(train.ts)
head(valid.ts)
length(valid.ts)
tail(train.ts)
tail(valid.ts)

# Regression Model 
Regression_Model = tslm(train.ts ~ trend + season, lambda = 0)
summary(Regression_Model)
forecast(Regression_Model, h = nvalid)

# fit the model on entire dataset and forecast the January 2002, and February 2002
Regression_Model1 = tslm(SouvenirSales.ts ~ trend + season, lambda = 0)
forecast(Regression_Model1, h = 3)

summary(Regression_Model1)

# b.	Using the training period, create an ACF plot until lag-15 for the forecast errors. 
acf(Regression_Model$residuals, lag.max = 15, main = "Acf plot of Lag 15")

# Now fit an AR model with lag-2 [AR (2)] to the forecast errors. 
Ar2 = arima(Regression_Model$residuals, order = c(2,0,0))
summary(Ar2)

Ar2_1 = arima(Regression_Model1$residuals, order = c(2,0,0))
summary(Ar2_1)

# Compute forecasts for January 2001, January 2002, and February 2002, using the regression and AR (2) index {AR(2)} models.
# for January 2001
forecast_jan = forecast(Regression_Model, h=1)
Error_forecast_jan = forecast(Ar2, h=1)

forecast_jan_2001 = forecast_jan$mean + Error_forecast_jan$mean
forecast_jan_2001

# January 2002, and February and march 2002
forecast_2002 = forecast(Regression_Model1, h=3)
Error_forecast_2002 = forecast(Ar2_1, h=3)

forecast_jan_feb_2002 = forecast_2002$mean + Error_forecast_2002$mean
forecast_jan_feb_2002
