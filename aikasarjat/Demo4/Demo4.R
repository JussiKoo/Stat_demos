#T1

empl <- read.csv2("empl.csv", header=TRUE)

osuus <- empl$unempl/empl$population

ue <- ts(osuus, start=1993, frequency=12)

ts.plot(ue)

ts.plot(diff(ue))

#===============================================================================

#T2