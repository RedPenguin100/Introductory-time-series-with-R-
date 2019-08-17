# Exercise 1

l = 1000
x <- w <- rexp(l) - 1
hist(x)
acf(x)

# Exercise 2 
# We want to simulate AR(1) process.
rm(list=ls())
set.seed(1) # Eliminate randomness
l = 100
w <- rnorm(l)
get_ar1 <- function (alpha, w) {
  x <- w
  for (t in 2:100) x[t] = alpha * x[t-1] + w[t]
  x
}

print_ar1_coeff <- function(alpha, w) {
  ar.process <- get_ar1(alpha, w)
  ar(ar.process)
}

library(nls2)
print_ar1_coeff_ver2 <- function(alpha, w) {
  x <- get_ar1(alpha, w)
  data <- data.frame(time= 1:l, x = x)
  fit <- nls2(x ~ c * exp((alpha - 1)*time), start=data.frame(alpha=c(0.95, 1.05), c= c(-10, 10)),
              algorithm="random-search", data=data, maxiter=100)
  fit
  
}
# All guessed correctly, as AR(1) process.
# a) simulation of series and estimation
print_ar1_coeff(alpha=0.5, w) # alpha.est = 0.4347, sigma^2 is 0.8184
print_ar1_coeff(alpha=0.9, w) # alpha.est = 0.7562, sigma^2 estimation = 0.8119
print_ar1_coeff(alpha=-0.5, w) # alpha.est = -0.5028, sigma^2 estimation = 0.8145
print_ar1_coeff(alpha=-0.9, w) # alpha.est = -0.8805, sigma^2 estimation = 0.8113
# b) simulation of series and estimation
# It looks like the ar function either does not support non stationary series or I am wrong somewhere.
print_ar1_coeff_ver2(alpha=1.01, w) # alpha.est = 0.961, sigma^2 was estimated as 2.778
print_ar1_coeff_ver2(alpha=1.02, w) # alpha.est = 0.9636, sigma^2 was estimated as 7.903
print_ar1_coeff_ver2(alpha=1.05, w) # alpha.est = 0.9464, sigma^2 estimated as 629.6



predict_ar_values <- function(steps, alpha, w) {
  x <- get_ar1(alpha, w)
  for (i in 1:steps) {
    x <- append(x, alpha * x[length(x)])
  }
  x
}
steps = 10
length(predict_ar_values(steps=steps, alpha=0.5, w=w))

# Printing the predictions: 
# a) predictions
plot(1:(l+steps), predict_ar_values(steps=steps, alpha=0.5, w=w))
plot(1:(l+steps), predict_ar_values(steps=steps, alpha=0.9, w=w))
plot(1:(l+steps), predict_ar_values(steps=steps, alpha=-0.5, w=w))
plot(1:(l+steps), predict_ar_values(steps=steps, alpha=-0.9, w=w))

# b) predictions
# The higher the alpha the smoother the predictions seem. But then 
# also the error is less significant in comparison to the exponential growth.
plot(1:(l+steps), predict_ar_values(steps=steps, alpha=1.01, w=w))
plot(1:(l+steps), predict_ar_values(steps=steps, alpha=1.02, w=w))
plot(1:(l+steps), predict_ar_values(steps=steps, alpha=1.05, w=w))

# 3 is given in external sheets
# 4 
rm(list=ls())
set.seed(1)
l = 1000
x <- w <- rnorm(l)
for (t in 3:1000) x[t] = 5/6 * x[t-1] - 1/6 *x[t-2] + w[t]
acf(x) # Steadily declining correlation, some odd spike in lag=16 but seems withing the dotted lines
pacf(x) # Only negative correlation in the first lag, unexpected for me. Would have thought that the first lag
        # would have positive correlation and the second lag might as well / very small negative correlation. 
# I skipped the rest for now because I don't think fitting the ar model using the ar function when the alpha is larger than 1. 


# 5
# c
rm(list=ls())
set.seed(1)
l=1000
x <- w <- rnorm(l)
for (t in 3:1000) x[t] = 3/2 * x[t-1] - 1/2 * x[t-2] + w[t]
y <- vector(mode="double", length=l-1)
for (t in 2:1000) y[t-1] = x[t] - x[t-1]
# d

is_inside_confidence_interval <- function(model.mean, sample.mean, sample.deviation, sample.size, confidence=0.95) {
  error <- qnorm(confidence) * sample.deviation / sqrt(sample.size)
  left <- sample.mean - error
  right <- sample.mean + error
  model.mean < right && left < model.mean
}
model = ar(y)
is_inside_confidence_interval(model$ar, sample.mean=0.5, sample.deviation=sd(y), sample.size=l)
acf(y) #  Decaying correlation, as we would expect from AR(1) model. Decays quickly beause the coefficient is small (1/2).
plot(model$resid[2:l]) # The plot looks like random gaussian distribution, which means the modelling was good.
acf(model$resid[3:l-1]) # This way it removes the first and last number, which are NA. No correlations between lags, 
# this confirms our initial assumption that the modelling was good.

# 6
rm(list=ls())
www <- "https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/global.dat"
Global.dat <- scan(www)
Global.ts <- ts(Global.dat, st=c(1856, 1), end=c(2005, 12), fr=12)
Global.agg <- ts(aggregate(Global.ts, FUN=mean))
Global.ar <- ar(Global.agg, method="mle")
Global.ar$order
Global.ar$ar

x <- vector(mode="double", length=length(Global.agg))
for (t in 1:4) x[t] <- Global.agg[t]
for (t in 5:length(Global.agg)) x[t] <- -0.14 + 0.59 * (x[t-1] + 0.14) + 0.013 * (x[t-2] + 0.14) + 0.11 * (x[t-3] + 0.14) + 0.27 * (x[t-4] + 0.14)
resid <- diff(x)
# I'm not sure what have I dont wrong but it does not come out like the residual series of the aggregate global temp time series.
plot(Global.ar$resid)
# b
acf(x)
acf(Global.agg) # Decay in correlation over time. The decay is not linear, and has ups and downs. 
pacf(Global.agg) # Slight correlation in the second and third lag, which is odd. You would expect the first and 4th term to have higher
# correlation, given the fitted series.

# c
future.temp <- predict(Global.ar, n.ahead = 12 * 100)
future.temp.agg <- aggregate(future.temp$pred, FUN=mean)
# d
new.and.old.temp <- append(Global.agg, future.temp.agg)
length(Global.agg) 
plot(seq(from=1856, by=1, length.out = length(new.and.old.temp)), new.and.old.temp, type="l", col="red")
lines(seq(from=1856, by=1, length.out = length(Global.agg)), Global.agg, ,type="l", col="black")
# e
abline(h=mean(Global.agg), col="blue")
