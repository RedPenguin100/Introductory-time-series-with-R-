www <- "https://raw.githubusercontent.com/svkerr/R_Files/master/TimeSeries/Maine.dat"
Maine.month <- read.table(www, header = TRUE)

attach(Maine.month)
class(Maine.month)
Maine.month.ts <- ts(unemploy, start = c(1996, 1), freq = 12)
Maine.annual.ts <- aggregate(Maine.month.ts) / 12
layout(1:2)
plot(Maine.month.ts, ylab="unemployed (%)")
plot(Maine.annual.ts, ylab= "unemployed (%)")

# The window function will help us to create the February month from our ts object. 
Maine.Feb <- window(Maine.month.ts, start=c(1996, 2), freq=TRUE)
Maine.Aug <- window(Maine.month.ts, start=c(1996, 8), freq=TRUE)

Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts)
Feb.ratio
Aug.ratio

