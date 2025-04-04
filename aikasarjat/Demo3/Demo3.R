#T1

#a

data_eq <- read.csv2("eq.csv", header=TRUE, sep = ",")

eq_ts <- ts(data_eq$Quakes, start=1900, freq=1)

#b

ts.plot(eq_ts)

diff_eq_ts <- diff(eq_ts)

ts.plot(diff_eq_ts)

#T2

