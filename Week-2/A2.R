
# Load the dataset
MBP = read.csv("Missouri Beer Production.csv", header = FALSE)
str(MBP)
colnames(MBP) = c("Year","Sales")
str(MBP)
# Convert "Year" column to Date format
MBP$Year <- as.Date(paste0("01-", MBP$Year), format = "%d-%b-%y")
# b.	Plot the dataset. Do you see any trend and seasonality? Very briefly Explain.
# Plot the data
plot(MBP, type = "l", xlab = "Time period", ylab = "Production Sales", main = "Missouri Beer Production")


# c.	Create a timeseries data using R ts() function with seasonal frequency 12 (months in a year) and another timeseries data with seasonal frequency of 4 (quarters in a year)
# Compare these graph and write down, your understanding. 
MBP_ts_Monthly=ts(MBP$Sales, start = c(2019,1), end = c(2023,12), frequency = 12)
par(mfrow=c(2, 1))
plot(MBP_ts_Monthly, main = "Missouri Beer Production (Monthly)")

MBP_ts_Quaterly = aggregate(MBP_ts_Monthly, nfrequency = 4, FUN = sum)
# Plot the quarterly time series  ` `
plot(MBP_ts_Quaterly, main = "Missouri Beer Production (Quarterly)")
par(mfrow=c(1, 1))

# d.	Choose the correct timeseries and partition the timeseries into training  and validation. Consider 75% and 25%.

# Data partitioning
nValid <- 15   # Here we have 60 time periods from that 25% is 15 time periods
length(MBP_ts_Monthly)
ntrain <- length(MBP_ts_Monthly) - nValid
train.ts <- window(MBP_ts_Monthly, start = c(2019, 1), end = c(2019, ntrain))
valid.ts <- window(MBP_ts_Monthly, start = c(2019, ntrain + 1), end = c(2019, ntrain + nValid))

# e.	Forecast validation with naïve, snaive, and trailing moving average. Use W = 5 for MA.
library(forecast)
Naive.Model = naive(train.ts, h = nValid)
SNaive.Model = snaive(train.ts, h= nValid)
library(zoo)
ma.trailing.Model = rollmean(train.ts, k = 5, align = "right")
last.ma <- tail(ma.trailing.Model, 1)

ma.trailing.pred <- ts(rep(last.ma, nValid), start = c(2019, ntrain + 1),
                       end = c(2019, ntrain + nValid), freq = 12)



# f.	Which one is a better model.
# Comparing three models with their accuracy
accuracy(Naive.Model,valid.ts)
accuracy(SNaive.Model, valid.ts)
accuracy(ma.trailing.pred, valid.ts)


# g.	Forecast January 24 with naïve, snaive, and moving average.
forecast(naive(valid.ts, h=1))
forecast(snaive(valid.ts,h=1))
forecast(rollmean(valid.ts, h=1, k=5, align = "right"))


# h.	Put training data, validation, and all forecasted data in one plot.
plot(train.ts, ylim = c(2000, 6000), ylab = "Sales", xlab = "Time Period", bty = "l", xaxt = "n", 
     xlim = c(2019, 2024.25), main = "Forecast Comparison", lty = 2, col = "dark red")
axis(1, at = seq(2019, 2024, 1), labels = format(seq(2019, 2024, 1)))

# Add validation data
lines(valid.ts, col = "blue")

# Add Naive Model
lines(Naive.Model$mean, lwd = 2, col = "dark blue")

# Add SNaive Model
SNaive.forecast <- forecast(SNaive.Model, h = nValid)
lines(SNaive.forecast$mean, col = "green")

# Add trailing moving average
lines(ma.trailing.pred, col = "orange")

# Add legend
legend("topleft", legend = c("Validation", "Naive Model", "SNaive Model", "Trailing MA"), 
       col = c("blue", "dark blue", "green", "orange"), lty = c(1, 1, 1, 1), cex = 0.8)


# Arrows for train, valid, and future periods
arrows(2019.1, 5000, 2022.7, 5000, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2022.9, 5000, 2023.12, 5000, code = 3, length = 0.1, lwd = 1, angle = 30)
arrows(2023.10, 5900, 2025, 5900, code = 3, length = 0.1, lwd = 1, angle = 30)

# Add text labels above arrows
text(2021, 6100, "Training")
text(2022.625, 6100, "Validation")
text(2024, 6100, "Future")



# i.	Get the RMSE, MAE, and MAPE. Which model gives a better performance.
accuracy(Naive.Model,valid.ts)
accuracy(SNaive.Model, valid.ts)
accuracy(ma.trailing.pred, valid.ts)
