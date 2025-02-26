# _________________________Assignment-7____________________

# _________________________Question-1_______________________

# Required libraries
library(tsfeatures)
library(forecast)

# 1.	Read the retail_index.csv

Retail_Index = read.csv("retail_index.csv")
str(Retail_Index)
head(Retail_Index)
tail(Retail_Index)

#2.	Prepare proper timeseries dataset 

Retail.ts = ts(Retail_Index$euretail, start = c(1996,1), end = c(2011,4), frequency = 4)
plot(Retail.ts)


# A.	Timeseries exploration
# a.	Use the decompose() to Extract the residuals, trend, and seasonality values from timeseries data
Retail_decompose = decompose(Retail.ts)
Retail_decompose$seasonal
Retail_decompose$trend
Retail_decompose$random

plot(Retail_decompose)

# b.	Plot the data and timeseries. 
# c.	Plot difference of lag 4, ACF and PACF of lag of 18. Interpret these plots

diff_4 = diff(Retail.ts, lag = 4)
plot(diff_4)
" The differencing will make timeseries data stationary"

# ACF and PACF of lag of 18.
par(mfrow=c(1,2)) 

acf(Retail.ts, lag.max = 18, main = "ACF plot for Retail Sales") 
# In acf plot there is decrease nature between the lags which means the time series is a non-stationary data which has trend or seasonality

pacf(Retail.ts, lag.max = 18, main = "PACF plot for Retail Sales") 
par(mfrow=c(1,1)) 

# B.	Building Models
# a.	Build the arima model with Arima() with the following setting:
# P = 0, d = 1, q = 1 and P = 0, D = 1, Q = 1 
Arima_Model = Arima(Retail.ts, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 4))

# b.	Plot the residual as well as ACF and PACF of residuals for 18 lags. Interpret the model
plot(Arima_Model$residuals)
acf(Arima_Model$residuals, lag.max = 18)
pacf(Arima_Model$residuals, lag.max = 18)

# c.	Build a new arima model with auto.arima(). Get this model setting and use Arima() with this new setting.  
AutoArima_Model = auto.arima(Retail.ts)
AutoArima_Model

# Get this model setting and use Arima() with this new setting.
Arima_Model_Auto = Arima(Retail.ts, order = c(0, 1, 3), seasonal = list(order = c(0, 1, 1), period = 4))

# Plot the new model residual as well as ACF and PACF of residuals for 18 lags. Interpret the model
plot(Arima_Model_Auto$residuals)
acf(Arima_Model_Auto$residuals, lag.max = 18)
pacf(Arima_Model_Auto$residuals, lag.max = 18)

# d.	Forecast for three years in the future. Plot the forecast.
Forecast_3Years = forecast(Arima_Model_Auto,h=12)
Forecast_3Years
plot(Forecast_3Years)



# ________________________Question-2_______________________________
library(caret)
# read the file
PME = read.csv("PowderyMildewEpidemic.csv")
head(PME)
tail(PME)

# 1.	In order for the model to serve as a forewarning system for farmers, what requirements must be satisfied regarding data availability?
# ANS: Maximum Temperature and Relative Humidity data must be available at time of prediction. These two predictors are used to fit the model.

# 2.	Write an equation for the model fitted by the researchers in the form of equation (8.1). Use predictor names instead of x notation.
# Equation:  log⁡(Odds) = β0 + β1 * Max.temp + β2 * Rel.humidity + ε

# 3.	Create a scatter plot of the two predictors, using different hue for epidemic and non-epidemic markers. 
# Does there appear to be a relationship between epidemic status and the two predictors?

'From the given data there is no relationship between epidemic status and the two predictors'
'In general, there should be relationship'

plot(PME$Max.temp, PME$Rel.humidity, 
     col = ifelse(PME$Outbreak == "Yes", "red", "blue"),
     main = "Scatter Plot of Max Temperature and Relative Humidity",
     xlab = "Max Temperature",
     ylab = "Relative Humidity",
     pch = 17)

# Add legend
legend("topright", legend = c("No Outbreak", "Outbreak"), 
       col = c("blue", "red"), pch = 17)


# 4.	Compute naive forecasts of epidemic status for years 1995-1997 using next-year forecasts (Ft+1 = Ft ). 

# Extract the epidemic status for the years 1995-1997
epidemic_status_1994 <- PME$Outbreak[PME$Year == 1994]
epidemic_status_1995 <- PME$Outbreak[PME$Year == 1995]
epidemic_status_1996 <- PME$Outbreak[PME$Year == 1996]
epidemic_status_1997 <- PME$Outbreak[PME$Year == 1997]

# naive forecasts of epidemic status for years 1995-1997 using next-year forecasts (Ft+1 = Ft )
naive_forecast_1995 <- epidemic_status_1994
naive_forecast_1996 <- epidemic_status_1995
naive_forecast_1997 <- epidemic_status_1996


# What is the naive forecast for year 2000? Summarize the results for these four years in a classification matrix.
naive_forecast_2000 <- epidemic_status_1997
naive_forecast_2000

# Summarize the results for these four years in a classification matrix.
# Actual epidemic status for years 1995-1997 and 2000
actual_status <- c("No", "Yes", "No", "No")

# Create a data frame with actual and forecasted epidemic status
classification_matrix <- data.frame(
  Year = c(1995, 1996, 1997, 2000),
  Actual = actual_status,
  Naive_Forecast = c(naive_forecast_1995, naive_forecast_1996, naive_forecast_1997, naive_forecast_2000)
)

# Print the classification matrix
print(classification_matrix)

# Confusion matrix
confusionMatrix(as.factor(PME[9:12,]$Outbreak), as.factor(classification_matrix$Naive_Forecast))


'Year Actual Naive_Forecast
1 1995     No            Yes
2 1996    Yes             No
3 1997     No            Yes
4 2000     No             No
'


# Therefore, out of the four years, only one prediction (for the year 2000) matches the actual outcome. 
# Hence, the accuracy of the naive forecasts is 25%.

# 5.	Partition the data into training and validation periods, so that years 1987-1994 are the training period. 

# convert Outbreak from categorical to Numeric
PME$Outbreak = as.numeric(PME$Outbreak == "Yes")
PME

# Data Partition
train.df = PME[1:8,]
validation.df = PME[9:12,]

# Fit a logistic regression to the training period using the two predictors.
Logistic_model = glm(Outbreak ~ Max.temp + Rel.humidity, data = train.df, family = "binomial")

# Report the outbreak probability as well as a forecast for year 1995 (use a threshold of 0.5). 
Predict_Valid = predict(Logistic_model, validation.df, type = "response")
Predict_Valid = ifelse(Predict_Valid >= 0.5, 1, 0)
Predict_Valid
'forecasted for year 1995 is the first predicted value i.e "0" '


# 6.	Generate outbreak forecasts for years 1996, 1997 and 2000 by repeatedly moving the training period forward. 
# For example, to forecast year 1996, partition the data so that years 1987-1995 are the training period. 
train_1.df = PME[1:9,]
validation_1.df = PME[10:12,]

# Then fit the logistic regression model and use it to generate a forecast (use threshold 0.5).
Logistic_model_1 = glm(Outbreak ~ Max.temp + Rel.humidity, data = train_1.df, family = "binomial")

Predict_Valid_1 = predict(Logistic_model, validation_1.df, type = "response")
Predict_Valid_1 = ifelse(Predict_Valid_1 >= 0.5, 1, 0)
Predict_Valid_1

# 7.	Summarize the logistic regression’s predictive accuracy for these four years (1995-1997, 2000) in a classification matrix.
validation.df$Outbreak = as.factor(validation.df$Outbreak)
Predict_Valid = as.factor(Predict_Valid)
confusion_Matrix = confusionMatrix(validation.df$Outbreak,Predict_Valid, positive = "1")

confusion_Matrix


# 8 & 9.	For the year 1997, there is some uncertainty regarding the data quality of the outbreak status. According to the logistic regression model, is it more likely that an outbreak occurred or not? 
# According to logistic regression model, yes it is more likely that an outbreak occurred.

# 10.	If we fit a logistic regression with a lag-outbreak predictor such as log(odds)t = β0 + β1(Outbreak)t-1 to years 1987-1997, how can this model be used to forecast an outbreak in year 2000?
Logistic_model_2 = glm(Outbreak ~ lag(Outbreak,1), data = train_1.df, family = "binomial")
Logistic_model_2
