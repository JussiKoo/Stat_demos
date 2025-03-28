#T1

#a

data(sunspots)
sunspots

exp_smooth <- function(x, b) {
  (1-b)*stats::filter(x, b, method="r", init=x[1]/(1-b))
}

exp_sunspots <- exp_smooth(sunspots, 0.8)

plot(exp_sunspots)

ts.plot(sunspots, exp_sunspots, gpars=list(xlab="vuosi", ylab="sunspots", 
                                         col=c("black","red")))

#b

p <- spec.pgram(sunspots, detrend=F, demean=T, taper=0.0)
plot(p)

#c

max(p$freq)
which.max(p$freq)

#Tyypillisin aika auringonpilkkujen välillä on siis 6 vuotta.
#Tämä on siis se taajuus joka on signaalissa "voimakkain".

#===============================================================================

#T2

load("bearings2.RData")

