# 1 a)
f <- function(k){
   w <-1:100
   x <- w + k * rnorm(100)
   y <- w + k * rnorm(100)
   ccf <- ccf(x,y)
}
f(1) # Almost perfect linear correlation, decreasing gradually as the lag increases.
f(10) # Less perfect linear correlation, decreases at a similar pace.
f(100) # No correlation. Only two samples leave the blue lines, which is evidence for the null hypothesis.
# 1 b)
Time <- 1:370
x <- sin(2 * pi * Time / 37)
y <- sin(2 * pi * (Time + 4) / 37)
ccf <- ccf(x,y) # Well, x is the same as y only at lag 4, where we have perfect correlation.
# Obviously the sin function will have a cosine correlogram with itself, here it is moved by 4 lags.
ccf[4] # equals to 0.996, which is basically 1 aside from floating point errors. 
# Let's add some noise!
x_rand <- x + rnorm(370)
ccf(x_rand,y) # Almost the same correlogram, but much less correlated, with about 50% decrease.
x_rand <- x + 10 * rnorm(370)
ccf(x_rand, y) # Fails to disprove the null hypothesis, interestingly enough still in the cosing fashion.

# 2 

# Will be presented in written form

# 3
# a - will be presented in written form
# b 
F <- function(t, mu, b) {
  1 / (1 + exp(-(t - mu) / b))
}
t = seq(0, 10, by = 0.01)
plot(t, F(t, 0, sqrt(3) / pi), type="l", col="red")
par(new=TRUE)
plot(t, pnorm(t), ,type="l", col="black") # Almost identical!

# c
# Will be presented in written form

# 4
# Will be presented in written form
BassModel <- function(t,p,q){
  ((1 - exp(-(p+q) * t)) / (1 + (q / p) * exp(-(p+q) * t)))
}
t = 1:365
y <- BassModel(t,0.01, 0.001)
pdf = diff(y) * 1000
plot(t, y, type="l")

# 5
# Will be presented in written form

# 6
# a
rm(list=ls())
www <- "https://raw.githubusercontent.com/AtefOuni/ts/master/Data/wine.dat"
wine.dat <- read.table(www, header=T); attach(wine.dat)
sweetw.ts <- ts(sweetw, start=c(1980, 1), freq=12)
plot(sweetw.ts, xlab="Year", ylab="Sales (1000 litres)")
sweetw.hw <- HoltWinters(sweetw.ts, seasonal="mult")
sweetw.hw.fixed <- HoltWinters(sweetw.ts, alpha=0.2, beta=0.2, gamma=0.2, seasonal="mult")
sweetw.hw$SSE       # 477693.9
sweetw.hw.fixed$SSE # 651777
# b
log.sweet.ts <- log(sweetw.ts)
sweet.hw.log <- HoltWinters(log.sweet.ts, seasonal="mult")
sweet.hw.log$SSE # 7.61344. Obviously it is radically different.
# c
next.month.sales <- tail(predict(sweetw.hw, n.ahead=1), n=1)
last.month.sales <- tail(sweetw.hw$x, n=1)
new.SSE <- (next.month.sales - last.month.sales) ^ 2 + sweetw.hw$SSE # 477695.69
sweet.ts.new <- append(sweetw.ts, last.month.sales)
sweet.hw.new <- HoltWinters(ts(sweet.ts.new,start=c(1980, 1), freq=12), seasonal="mult")
sweet.hw.new$SSE # 477695.7 - actually pretty good!

# d
basic.ts <- ts(sweetw, start=c(1980,1), end=c(1981, 12), freq=12) # Exactly 2 periods, needed for HW function.
basic.hw <- HoltWinters(basic.ts, seasonal="mult")
basic.hw$SSE # Starting point - 2524.
for (n in 24:187) {
  # Predict the next value
  next.value <- predict(basic.hw, n.ahead = 1)
  # Reoptimize everything
  basic.ts <- append(basic.ts, next.value)
  basic.ts <- ts(basic.ts, start=c(1980, 1), freq=12)
  print(c(n,  basic.hw$SSE, next.value))
  basic.hw <- HoltWinters(basic.ts, seasonal="mult") # Gets an internal error at n=60, but all the SSE are the same as the first.
}
basic.hw$SSE

# 7
# a
www <- "https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st=c(1856,1), end=c(2005, 12), fr=12)
plot(Global.ts) # Here you can see a rise in tempretaure, but it is not very clear that 
                # it would stay this way because of heavy seasonal variation
plot(aggregate(Global.ts, FUN = mean)) # Annual aggregate  
                                       # Here the upward trend is much clearer
boxplot(Global.ts ~ cycle(Global.ts)) # boxplot
                                      # The only major thing I notice is that the upper hemisphere summer months 
                                      # tend to be with much less variation. 
                                      
# b
Global.decom <- decompose(Global.ts)
plot(Global.decom)
# Superimposed seasonal effect
ts.plot(cbind(Global.decom$trend, Global.decom$trend + Global.decom$seasonal), lty=1:2)
# c
Global.decom$random
Global.acf <- acf(Global.decom$random[7:(length(Global.decom$random) - 6)], main="")
# The easiest spike to explain is at lag=1, it seems that if a random change happened 
# it is somewhat likely to slip over to the next month as well. 
# There is also unclear seasonal effect, when after a little less than half a yaer
# There is a strong negative correlation, indicating change of season. 
# What is wierd is that the first seasonal spike is obtained at lag=10, but the second spike 
# is more logical, obtained at lag=24, exactly 2 years. 

# d
# I'm not a climate scientists, so I'm not sure how increasing trend would affect 
# seasonal effects. It seems logical that the weather will be more extreme, but has 
# significantly worse results, so I'll stick with the addative seasonal effects.
Global.hw <- HoltWinters(Global.ts)
plot(Global.hw)
Global.hw$alpha
Global.hw$beta
Global.hw$gamma

# e
months <-  5 * 12
future.temp <- predict(Global.hw, n.ahead = months)
future.global.ts <- ts(append(Global.ts, future.temp), start=c(1856, 1), frequency=12)
ts.plot(future.global.ts,Global.ts, col=c("red", "black"))
# Under which circumstances would these forecasts be valid?
# The forecast is very wierd, it seems to have very little variance. 
# I would say it would be accurate by the mean, and not as the value by itself.

# The comments of caution I would make to a politician is that we assume in this model
# that the rate of changes remains similiar to what there is now. If we start radically 
# producing / reducing greenhouse gasses this would invalidate the trend, and obviously 
# this does not model extreme events like asteroid hit or nuclear reactor explosion etc.


# 8
www <- "https://raw.githubusercontent.com/AtefOuni/ts/master/Data/motororg.dat"
Motor.dat <- read.table(www, header = T); attach(Motor.dat)
Comp.ts <- ts(complaints, start = c(1996, 1), freq=12)
CUSUM <- function(ts, tau) {
  cumsum(ts) - length(ts) * tau
}
CUSUM(Comp.ts, 18)

# 9
Comp.ts.missing <- ts(complaints, start=c(1996,1), end=c(1999, 11), freq=12)
Comp.hw.miss1 <- HoltWinters(Comp.ts.missing, alpha=0.01, beta=F, gamma=F)
Comp.hw.miss2 <- HoltWinters(Comp.ts.missing, alpha=0.99, beta=F, gamma=F)
x.hat1 <- predict(Comp.hw.miss1, n.ahead=1)
x.hat2 <- predict(Comp.hw.miss2, n.ahead=1)
Comp.hw1 <- HoltWinters(Comp.ts, alpha=0.01, beta=F, gamma=F)
Comp.hw2 <- HoltWinters(Comp.ts, alpha=0.99, beta=F, gamma=F)
# This was the closest thing to the question I could interpert. 
# e_t is not available anywhere.
if (Comp.hw1$fitted[length(Comp.hw1$fitted)] - x.hat1 == 0){
  print("Yay!")
}
if (Comp.hw2$fitted[length(Comp.hw2$fitted)] - x.hat2 == 0){
  print("Yay!")
}
# Redraw the figures:
plot(Comp.hw1)
plot(Comp.hw2)
# For low alpha values we have very unresponsive process, with long memory. 
# It is basically a long line that gets very slowly modified, and almost not at all 
# when the mean is constant. 

# For high alpha values we are constantly updating, which basically renders our process
# almost the same, only that we lag by 1. 