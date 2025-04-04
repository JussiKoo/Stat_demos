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

span <- 765

spec.pgram(good1, detrend=F, spans=span, demean=T, taper=0.0)
spec.pgram(good2, detrend=F, spans=span, demean=T, taper=0.0)
spec.pgram(good3, detrend=F, spans=span, demean=T, taper=0.0)

spec.pgram(faulty1, detrend=F, spans=span, demean=T, taper=0.0)
spec.pgram(faulty2, detrend=F, spans=span, demean=T, taper=0.0)
spec.pgram(faulty3, detrend=F, spans=span, demean=T, taper=0.0)

#Tasoitus helpottaa vertailua, piikit näkyy selkeämmin

#c

cpgram(good1, taper=0)
cpgram(good2, taper=0)
cpgram(good3, taper=0)
cpgram(faulty1, taper=0)
cpgram(faulty2, taper=0)
cpgram(faulty3, taper=0)

#Jokaisessa hyvässä laakerissa tapahtuu samanlainen "hyppy" noin 1500 Hz 
#kohdalla

#===============================================================================

#T3

#a

phi <- c(3/4, -1/4)

y_ar <- arima.sim(model=list(ar=phi), 500)

plot(y_ar)

#b

theta <- c(0.73, 0.26)

y_ma <- arima.sim(model=list(ma=theta), 500)

plot(y)

#c

y_arma <- arima.sim(model=list(ar=phi, ma=theta), 500, n.start = 1e5)

plot(y_arma)

#d

acf(y_ar, lag.max = 50)

acf(y_ma, lag.max = 50)

acf(y_arma, lag.max = 50)

#Joka aikasarjassa autokorrelaatiot ovat suuria pienillä viiveillä ja muuten
#melko pieniä. Jokaisessa sarjassa on myös useita autokorrelaatioita suuremmilla
#viiveillä, jotka ovat merkitseviä.

spec.pgram(y_ar, detrend=F, spans=7, demean=T, taper=0.0)

spec.pgram(y_ma, detrend=F, spans=7, demean=T, taper=0.0)

spec.pgram(y_arma, detrend=F, spans=7, demean=T, taper=0.0)

#Periodogrammeissa ei ole kauheasti eroavaisuuksia. Matalia taajuuksia aikasar-
#joissa esiintyy paljon.
