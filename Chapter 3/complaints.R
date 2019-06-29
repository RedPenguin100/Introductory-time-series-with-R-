rm(list=ls())
www <- "https://raw.githubusercontent.com/AtefOuni/ts/master/Data/motororg.dat"
Motor.dat <- read.table(www, header = T); attach(Motor.dat)
Comp.ts <- ts(complaints, start = c(1996, 1), freq=12)
plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")
Comp.hw1 <- HoltWinters(complaints, beta = F, gamma = F) ; Comp.hw1
Comp.hw1$SSE

# I think it still does seasonal adjustmant because of the usage in the `decompose` function
Comp.hw1 <- HoltWinters(Comp.ts, alpha=0.2, beta = F, gamma = F) ; Comp.hw1
Comp.hw1$SSE

