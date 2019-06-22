www <- "https://raw.githubusercontent.com/AtefOuni/ts/master/Data/wave.dat"
wave.dat <- read.table(www, header=T)
attach(wave.dat)
plot(ts(waveht))
plot(ts(waveht[1:60]))

plot(waveht[1:396], waveht[2:397])
acf(waveht)