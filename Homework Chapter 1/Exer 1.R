www <-  "https://raw.githubusercontent.com/kaybenleroll/data_workshops/master/ws_timeseries_201309/cbe.dat"
CBE <-  read.table(www, header=T)
CBE.choc <- CBE[,1]
CBE.choc.ts <- ts(CBE.choc, start=1958, freq=12)
plot(CBE.choc.ts, ylab="Chocolate in some unit measure.", "year")
aggregated_chocolate <- aggregate(CBE.choc.ts)
plot(aggregated_chocolate, ylab="Aggregated chocolate", xlab="year")
cycle(CBE.choc.ts)
boxplot(CBE.choc.ts ~ cycle(CBE.choc.ts))