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
