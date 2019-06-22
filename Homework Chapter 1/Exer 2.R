# 2
q0 <- c(0.33, 2000, 40, 3, 2)
p0 <- c(18000, 0.80, 40, 80, 200)
qt <- c(0.5, 1500, 20, 2, 1)
pt <- c(20000, 1.6, 60, 120, 360)

LI <- sum(q0 * pt) / sum(q0 * p0)
# 3 a
PI <- sum(qt * pt) / sum(qt * p0)
# 3 b
# PI is calculated based on the quantity at time t. People tend to move away from 
# items that show sharp increase in prices so they will get less of them in favor of other stuff.

# 3 c
sqrt(PI * LI)

a <- c(1,2,4,5,2,3,5,6,7)
sort.result <- sort(a)
