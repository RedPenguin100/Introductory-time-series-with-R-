www <- "https://raw.githubusercontent.com/AtefOuni/ts/master/Data/Fontdsdt.dat"
fontdsdt.dat <- read.table(www, header=T)
attach(fontdsdt.dat)
plot(ts(adflow), ylab='adflow')
acf(adflow, xlab='lag (months)', main="")