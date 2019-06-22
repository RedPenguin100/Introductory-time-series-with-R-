# 1a
www <- "https://github.com/AtefOuni/ts/blob/master/Data/varnish.dat"
varnish.dat <- read.table(www, header=T)
plot(varnish.dat) 
cor(varnish.dat)
# It seems like a quadratic relation around x=5. The correlation is not high because we measure it linearly. 


# 1b
www <- "https://raw.githubusercontent.com/AtefOuni/ts/master/Data/guesswhat.dat"
guesswhat.dat <- read.table(www, header=T)
guesswhat.dat
plot(guesswhat.dat)
cor(guesswhat.dat)
# Correlation is not high, but it seems like we have a non linear relationship. If we can transform x properly we will get a 
# high correlation. I'm not sure what it represents, butall the number are integers, almost all non repeating and vary from one another
# in usually small steps, it seems like rounded measurements of some natural process with 2 noticeable errors.

# 2
www <- "C:\\Users\\Michael\\Documents\\Learning\\Masters in mathematics\\Thesis\\Introductory-time-series-with-R-\\Chapter 2\\ch2ex2.dat"
wines <- read.table(www, header=T)
attach(wines)
shiraz.ts = ts(shiraz)
cagey.ts = ts(cagey)
# 2a
ts.plot(shiraz.ts)
ts.plot(cagey.ts)
# 2b 1 lag scatter plots
plot(shiraz.ts[1:15], shiraz.ts[2:16])
plot(cagey.ts[1:15], cagey.ts[2:16])
# 2c
# exponentially decreasing, typical of AR(1) process as the author noted in page 41
acf(shiraz.ts)
# Exponentially decreasing, but negative correlation with the next term. Perhaps this is also 
# an AR(1) process only with a negative term.
acf(cagey.ts)

# 3
www <- "https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st=c(1856, 1), end = c(2005, 12), fr=12)
# 3a
Global.decom <- decompose(Global.ts)
plot(Global.decom)
# I would expect this data to have substantial seasonal component.
sd(Global.ts)
sd(Global.ts - Global.decom$seasonal) # Slightly less than the one with season component.
# superimposed seasonal effect
ts.plot(cbind(Global.decom$trend, Global.decom$trend + Global.decom$seasonal), lty=1:2)
# 3b
l <- length(Global.decom$random)
# First and last 6 elements are missing
acf(Global.decom$random[7:(l - 6)], xlab="Temperature", main="Global warming correlogram")

# 4

rm(list=ls())
www <- "https://raw.githubusercontent.com/AtefOuni/ts/master/Data/Fontdsdt.dat"
fontdsdt.dat <- read.table(www, header=T)
attach(fontdsdt.dat)
plot(ts(adflow), ylab='adflow')
acf(adflow, xlab='lag (months)', main="")
adflow.ts <- ts(adflow, fr=30)
adflow.ts 
adflow.decom <- decompose(adflow.ts)
plot(adflow.decom)
missing_amount <- 16
l <- length(adflow.decom$random)
acf(adflow.decom$random[missing_amount:(l - missing_amount)], main="Correlogram of adflow random component")
# They are similar, except here there is a tendency towards positive correlation after a certain lag rather than negative. 