www <- "https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st=c(1856, 1), end = c(2005, 12), fr=12)

# We can avoid explicitly dividing by 12 if we specify the FUN
Global.annual <- aggregate(Global.ts, FUN=mean)
plot(Global.ts)
plot(Global.annual)

New.series <- window(Global.ts, start=c(1970, 1), end=c(2005, 12))
New.time <- time(New.series)

class(New.time)
plot(New.series); abline(reg=lm(New.series ~ New.time))

