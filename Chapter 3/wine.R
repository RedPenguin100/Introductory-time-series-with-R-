rm(list=ls())
www <- "https://raw.githubusercontent.com/AtefOuni/ts/master/Data/wine.dat"
wine.dat <- read.table(www, header=T); attach(wine.dat)
sweetw.ts <- ts(sweetw, start=c(1980, 1), freq=12)
plot(sweetw.ts, xlab="Year", ylab="Sales (1000 litres)")
sweetw.hw <- HoltWinters(sweetw.ts, seasonal="mult")
# The coefficients are the coefficients, the SSE is the error term(s).
sweetw.hw; sweetw.hw$coefficients; sweetw.hw$SSE
sqrt(sweetw.hw$SSE/length(sweetw))
sd(sweetw)
plot(sweetw.hw$fitted)
# The results show relatively high alpha and gamma, but the beta are low. This means the level and the seasonal variation adapt rapidly 
# but the trend is slow to do so.

# ........
AP.hw <- HoltWinters(AirPassengers, seasonal = "mult")
plot(AP.hw)
AP.predict <- predict(AP.hw, n.ahead = 4 * 12)
ts.plot(AirPassengers, AP.predict, lty=1:2)
