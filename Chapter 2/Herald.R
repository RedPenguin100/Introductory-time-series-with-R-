www <- "https://raw.githubusercontent.com/AtefOuni/ts/master/Data/Herald.dat"
Herald.dat <- read.table(www, header=T)
attach(Herald.dat)
x <- CO
y <- Benzoa
n <- length(x)
cov = sum(((x - mean(x))* (y - mean(y)))) / (n -1)
cov
cov(x, y)
sd(x) * sd(y)
cov(x, y) / (sd (x) * sd(y))
cor(x, y)