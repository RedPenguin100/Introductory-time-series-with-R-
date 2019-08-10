set.seed(1)
x <- w <- rnorm(1000)
for (t in 2:1000) x[t] <- x[t-1] + w[t]
plot(x, type="l")
acf(x)
acf(diff(x))
