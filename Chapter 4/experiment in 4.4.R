www <- "https://raw.githubusercontent.com/kaybenleroll/data_workshops/master/ws_timeseries_201309/pounds_nz.dat"
Z <- read.table(www, header=T)
Z.ts <- ts(Z, st=1991, fr = 4)
acf(diff(Z.ts))

# gamma=0 is not the correct syntax for removing seasonal effect. The book is incorrect. (Or uses 
# a different R version)
Z.hw <- HoltWinters(Z.ts, gamma=FALSE, alpha=1)
acf(resid(Z.hw))
Z.hw$alpha
Z.hw$beta
