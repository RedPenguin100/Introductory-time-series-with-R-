www <-  "https://raw.githubusercontent.com/kaybenleroll/data_workshops/master/ws_timeseries_201309/pounds_nz.dat"

Z <-read.table(www, header=T)
Z[1:4, ]

Z.ts <- ts(Z, start=1991, freq=4)

plot(Z.ts, xlab= "time / years", ylab= "New zeland currency to pounds")


Z.92.96 <- window(Z.ts, start=c(1992,1), end=c(1996, 1))
Z.96.98 <- window(Z.ts, start=c(1996,1), end=c(1998, 1))

layout(1:2)
plot(Z.92.96, ylab="Exchange rate in $NZ / pounds")
plot(Z.96.98, ylab="Exchange rate in $NZ / pounds")