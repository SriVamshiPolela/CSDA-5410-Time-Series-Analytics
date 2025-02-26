# ______________________________Assignment-8____________________________________
# Forecasting with Neural Network 


# a)	Would you consider neural networks for this task? Explain why.
# Yes, Neural networks is a good choice because they're great at understanding complicated patterns in data. 
# Since wine sales can be affected by lots of different things, neural networks can figure out how all those different factors come together to affect sales.

# read the file
wines.df = read.csv("AustralianWines.csv")
head(wines.df)
tail(wines.df)

# Convert it to time series data
# b)	Use neural networks to forecast fortified wine sales , as follows:
wines.ts = ts(wines.df$Fortified, start = c(1980,1), end = c(1994,12), frequency = 12)
plot(wines.ts)

# Partition the data using the period until December 1993 as the training period.
len_data = length(wines.ts) 
nvalid = 12
train_len = len_data - nvalid
train.ts = window(wines.ts, start = c(1980,1), end = c(1980, train_len))
valid.ts = window(wines.ts, start = c(1980,train_len+1), end = c(1980,train_len + nvalid))
train.ts
valid.ts

#	Run a neural network using R’s nnetar() with 11 non-seasonal lags (i.e., p = 11). 
# Leave all other arguments at their default

set.seed(2024)
wines.nnetar = nnetar(train.ts, p = 11)
summary(wines.nnetar)
wines.nnetar.pred = forecast(wines.nnetar, h = nvalid)
wines.nnetar.pred

# c)	Create a time plot for the actual and forecasted series over the training period. 
# Create also a time plot of the forecast errors for the training period. Interpret what you see in the plots.
plot(train.ts, xlab = "Time", ylab = "Wine Sales", xlim = c(1980, 1995), ylim = c(0,6000),
     main = "Forecasting Wine Sales")
lines(wines.nnetar$fitted, col = "red", lty = 2)
lines(wines.nnetar$residuals, col = 'green', lwd=2)
legend("topright", legend = c("Training", "fitted", "Residuals"), col = c("black", "red", "green"),lty = c(1, 2, 1))


# d)	Use the neural net to forecast sales in January and February 1995.
wines.nnetar_1994 = nnetar(wines.ts, p=11)
wines.nnetar.pred_1995 = forecast(wines.nnetar_1994, h = 2)
wines.nnetar.pred_1995

# e)	Use R’s ets() function to automatically select and fit an exponential smoothing model to the training period until December 1993. Which model did ets fit?
ets_model = ets(train.ts)
summary(ets_model)
plot(ets_model)

# Which model did ets fit?   
    # Model(M,A,M)

# f)	Use this exponential smoothing model to forecast sales in January and February 1995.
ets_forecast = forecast(ets_model, h=2)
ets_forecast


# g)	Compare and interpret the results
'The ETS model forecasts higher sales for January and February 1995 compared to NNETAR.
ETS also offers prediction intervals (80% and 95% confidence levels), highlighting forecast uncertainty, while NNETAR doesnot provide such intervals.'

' However, the uncertainty associated with the forecasts should also be considered when making decisions based on these predictions.'


