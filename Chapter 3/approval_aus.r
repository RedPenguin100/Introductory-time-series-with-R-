www <- "https://raw.githubusercontent.com/kaybenleroll/data_workshops/master/ws_timeseries_201309/ApprovActiv.dat"
Build.dat <- read.table(www, header=T); attach(Build.dat)
App.ts <- ts(Approvals, start=c(1996, 1), freq=4)
Act.ts <- ts(Activity, start=c(1996, 1), freq=4)
ts.plot(App.ts, Act.ts, lty=c(3,3))

acf(ts.union(App.ts, Act.ts))
app.ran <- decompose(App.ts)$random
app.ran.ts <- window(app.ran, start=c(1996, 3), end=c(2006,1))
act.ran <- decompose(Act.ts)$random
act.ran.ts <- window(act.ran, start=c(1996, 3), end=c(2006,1))

print(act.ran.ts)
acf(ts.union(app.ran.ts, act.ran.ts))
ccf(app.ran.ts, act.ran.ts)
print(acf(ts.union(app.ran.ts, act.ran.ts)))

