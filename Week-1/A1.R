
#############################################################################
# Question 2.	Generate 1000 random number with normal distribution
set.seed(2024)
random_numbers = rnorm(1000,0,1) # rnorm can 
# a.	Transform this dataset to time series using ts(). 
random_numbers = ts(random_numbers)
# b.	Plot the timeseries using plot()
plot(random_numbers, main="Timeseries Data")
# c.	What are start, end and the frequency of this time series?
start(random_numbers)
end(random_numbers)
frequency(random_numbers)
# d.	Show the series is white noise with normal distribution.
mean(random_numbers)
var(random_numbers)
hist(random_numbers)
plot(density(random_numbers)) # *******************************
# e.	Save your dataset as “white_noise.csv”
write.csv(random_numbers, file = "white_noise.csv", row.names = FALSE)


###########################################################################
# Question 3.	Open the “white_noise.csv” file. (10 Points)
# a.	Generate 500 observations which each observation represents the following autoregressive equation:
  
White_noise.df = read.csv("white_noise.csv")
AutoRegressive_WhiteNoise = stats::filter(White_noise.df, filter = c(0.4,-0.8), method = "recursive")[-(1:500)]
plot.ts(AutoRegressive_WhiteNoise, main = "AutoRegression")
write.csv(AutoRegressive_WhiteNoise, file = "autoregressive_2.csv", row.names = FALSE)


############################################################################
# Question 4.	Open the Open the “white_noise.csv” file (5 Points)
# a.	Generate 500 observation with the random walk feature 
White_noise.df = read.csv("white_noise.csv")
randomWalk = White_noise.df[1:500,]
x = cumsum(randomWalk)
randomWalk_Drift = randomWalk + 0.3
xd = cumsum(randomWalk_Drift)
# Plot both random walk and random walk with drift
plot.ts(xd, ylim=c(-5,55), main="Random Walk", xlab='')
lines(x, col=4)
abline(h=0, col=4, lty=2)
abline(a=0, b=0.3, lty=2)

############################################################################
# Question 5.  
cs<-5*cos(pi*1:500/25 + pi)
par(mfrow=c(2, 1))#partition the plot area in three section one plot in every section

# Without error
plot.ts(cs, main=expression(5*cos(pi*t/25 + pi)))
# With Error
w <-rnorm(500, 0, 1)# Generate 500 random values with mean = 0 and σ_w=1
plot.ts(cs+w, main=expression(5*cos(pi*t/25 + pi)+N(0,1)))

# Reset the plotting layout to default (one single plot)
par(mfrow=c(1, 1))


###########################################################################

# Problem 1
travel = read.csv("Sept11Travel.csv")
head(travel)
tail(travel)
# Remove commas in both Air.RPM_Thausends and Rail.PM
travel$Air.RPM_Thausends <- as.integer(gsub(",", "", travel$Air.RPM_Thausends))
travel$Rail.PM <- as.integer(gsub(",", "", travel$Rail.PM))

# Convert this data to time series data
travel = ts(travel[,-1], start = c(1990,1), end = c(2004,4), frequency = 12)
head(travel)
plot(travel, main = "Travel in the United States")

# Fit a linear trend line to the data and add it to the plot
trend <- lm(travel ~ time(travel))
abline(trend, col = "red")

# Suppressing Seasonality:
# To suppress seasonality, we can use seasonal decomposition
decomp <- decompose(travel)
trend_component <- decomp$trend
plot(trend_component, main = "Trend Component of Travel Data", ylab = "", xlab = "Year")



###########################################################################################
# Problem 5
SouvenirSales = read.csv("SouvenirSales.csv")
head(SouvenirSales)
tail(SouvenirSales)
TimeSeries_SouvenirSales = ts(SouvenirSales[,-1] , start = c(1995,1),end = c(2001,12) , frequency = 12)

# Plot the time series data
plot(TimeSeries_SouvenirSales, main = "Monthly Souvenir Sales (1995-2001)",
     xlab = "Date", ylab = "Sales", col = "blue", lwd = 2)


# Plot the time series data with logarithmic scale on both axes
plot(TimeSeries_SouvenirSales, main = "Monthly Souvenir Sales (1995-2001)",
     xlab = "Date", ylab = "Log Sales", col = "blue", lwd = 2,
     log = "xy")

