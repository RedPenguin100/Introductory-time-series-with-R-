www <-  "https://raw.githubusercontent.com/kaybenleroll/data_workshops/master/ws_timeseries_201309/cbe.dat"
CBE <-  read.table(www, header=T)
CBE[1:4,] # This prints the first 4 rows of the data frame
class(CBE)

Elec.ts <- ts(CBE[, 3], start=1958, freq=12)
Beer.ts <- ts(CBE[, 2], start=1958, freq=12)
Choc.ts <- ts(CBE[, 1], start=1958, freq=12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))


AP <- AirPassengers
AP.elec <- ts.intersect(AP, Elec.ts)
start(AP.elec)

end(AP.elec)
AP.elec[1:3, ] # Prints the first 3 rows of AP and of Elec.ts
AP <- AP.elec[, 1]; Elec <- AP.elec[, 2]
layout(1:2)
plot(AP, main = "", ylab = "Air Passengers / 1000's")
plot(Elec, main = "", ylab = "Electricity production / MkWh")

# This plot showes correlation betewen the AP and the Elec time series.
# as.vector functions are intended to convert the TS objects to ordinary vectors for the plot.
plot(as.vector(AP), as.vector(Elec), xlab= "Air passengers", ylab="Electricity production / Mwh")

# This draws an abline in the previous graph (kinda minimal square-ish)
abline(reg = lm(Elec ~ AP))

# Using decomposition methods of Time Series to actually view the seasonal effects and trend:
plot(decompose(Elec.ts))
Elec.decom <- decompose(Elec.ts, type="mult")
plot(Elec.decom)
Trend <- Elec.decom$trend
Seasonal <-  Elec.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty= 1 : 2)
