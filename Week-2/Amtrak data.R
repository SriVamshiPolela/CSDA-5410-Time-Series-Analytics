Amtrak.data <- read.csv("Amtrak data.csv")
library(forecast)
Amtrak.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
plot(Amtrak.ts , ylim = c(1300, 2600),  ylab = "Passenger Count", xlab = "Date", bty = "l", xaxt = "n", xlim = c(1991,2006.25), col="dark blue")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1), digits = 2))
lines(c(2004.25 -3 , 2004.25 -3), c(0, 3500), col="dark red")
lines(c(2004.25, 2004.25), c(0, 3500), col = "dark orange")
text(1996.25, 2500, "Training", col = "green")
text(2002.75, 2500, "Validation", col = "brown")
text(2005.25, 2500, "Future", col = "red")
arrows(2004 - 3,2450,1991.25,2450,code=3,length=0.1,lwd=1,angle=30)
arrows(2004.5 - 3,2450,2004,2450,code=3,length=0.1,lwd=1,angle=30)
arrows(2004.5,2450,2006,2450,code=3,length=0.1,lwd=1,angle=30)


#data partitioning
stepsAhead <- 36   #three years, 36 month
length(Amtrak.ts)
train.length <- length(Amtrak.ts) - stepsAhead
train.ts <- window(Amtrak.ts, start = c(1991, 1), end = c(1991, train.length))
valid.ts <- window(Amtrak.ts, start = c(1991, train.length + 1), end = c(1991, train.length + stepsAhead))
#Use regression function for time sereis tslm() to forcasting model with quadratic curve.
Amtrak.lm <-  tslm(train.ts ~ trend + I(trend^2)) # assuming a quadratic curve trend
Amtrak.lm.pred <- forecast(Amtrak.lm, h = stepsAhead, level = 0)#forcasting the validation data (h mean horizon)
## Generating the vizualization
plot(Amtrak.lm.pred, ylim = c(1300, 2600),ylab = "Passenger Count", xlab = "Date", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2, col = "red")
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1))) 
lines(Amtrak.lm$fitted, lwd = 2, col= "green")
lines(valid.ts)
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)
