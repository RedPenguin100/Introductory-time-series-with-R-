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
# All guessed correctly, as AR(1) process.
# a) simulation of series and estimation
print_ar1_coeff(alpha=0.5, w) # alpha.est = 0.4347, sigma^2 is 0.8184
print_ar1_coeff(alpha=0.9, w) # alpha.est = 0.7562, sigma^2 estimation = 0.8119
print_ar1_coeff(alpha=-0.5, w) # alpha.est = -0.5028, sigma^2 estimation = 0.8145
print_ar1_coeff(alpha=-0.9, w) # alpha.est = -0.8805, sigma^2 estimation = 0.8113
# b) simulation of series and estimation
# It looks like the ar function either does not support non stationary series or I am wrong somewhere.
print_ar1_coeff(alpha=1.01, w) # alpha.est = 0.961, sigma^2 was estimated as 2.778
print_ar1_coeff(alpha=1.02, w) # alpha.est = 0.9636, sigma^2 was estimated as 7.903
print_ar1_coeff(alpha=1.05, w) # alpha.est = 0.9464, sigma^2 estimated as 629.6



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
l = 1000
x <- w <- rnorm(l)
for (t in 3:1000) x[t] = 5/6 * x[t-1] - 1/6 *x[t-2] + w[t]

