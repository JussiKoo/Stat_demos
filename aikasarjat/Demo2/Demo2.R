#T1

#a

data(sunspots)
sunspots

sunspots <- ts(sunspots, frequency=12)

exp_smooth <- function(x, b) {
  (1-b)*stats::filter(x, b, method="r", init=x[1]/(1-b))
}

exp_sunspots <- exp_smooth(sunspots, 0.8)

ts.plot(sunspots, exp_sunspots, gpars=list(xlab="vuosi", ylab="sunspots", 
                                         col=c("black","red")))

#b

p <- spec.pgram(sunspots, detrend=F, demean=T, taper=0.0)

#c

#jaksonpituus = 1/taajuus

1/p$freq[which.max(p$spec)]

#Tyypillisin aika auringonpilkkujen välillä on siis noin 11 vuotta.

#===============================================================================

#T2

load("bearings2.RData")

good1 = ts(bearings$good1, frequency=97656)
good2 = ts(bearings$good2, frequency=97656)
good3 = ts(bearings$good3, frequency=97656)
faulty1 = ts(bearings$faulty1, frequency=97656)
faulty2 = ts(bearings$faulty2, frequency=97656)
faulty3 = ts(bearings$faulty3, frequency=97656)

spec.pgram(good1, detrend=F, demean=T, taper=0.0)

spec.pgram(good2, detrend=F, demean=T, taper=0.0)

spec.pgram(good3, detrend=F, demean=T, taper=0.0)

spec.pgram(faulty1, detrend=F, demean=T, taper=0.0)

spec.pgram(faulty2, detrend=F, demean=T, taper=0.0)

spec.pgram(faulty3, detrend=F, demean=T, taper=0.0)

#Hyvissä laakereissa on periodogrammissa piikki noin 15000 Hz kohdalla

#Viallisissa laakereissa on periodogrammissa piikki noin 2000 Hz kohdalla.

#b

spec.pgram(good1, detrend=F, spans=765, demean=T, taper=0.0)
spec.pgram(good2, detrend=F, spans=765, demean=T, taper=0.0)
spec.pgram(good3, detrend=F, spans=765, demean=T, taper=0.0)

spec.pgram(faulty1, detrend=F, demean=T, taper=0.0)

spec.pgram(faulty2, detrend=F, demean=T, taper=0.0)

spec.pgram(faulty3, detrend=F, demean=T, taper=0.0)


