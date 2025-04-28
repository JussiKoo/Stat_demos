#T1

f <- file("ah.wav", "rb")              # avataan tiedosto luettavaksi
hdr <- readBin(f, "raw", 44)           # luetaan otsikko-osa
x <- readBin(f, "int", size=2, n=4001) # luetaan data muuttujaan 'x'
close(f)                               # suljetaan tiedosto

#Tehdään aikasarjaobjekti

x <- ts(x, frequency=8000, start = 0)

#Piirretään aikasarja
ts.plot(x)

#Piirretään periodogrammi
spec.pgram(x)

#Piiretään otosautokorrelaatioita eri viiveillä
acf(x, lag.max = 2000)

#Piiretään osittaisia otosautokorrelaatioita eri viiveillä
pacf(x, lag.max = 2000)

#Sovitetaan signaaliin AR(100)-malli Yule-Walkerin estimoinnilla
#fit <- ar.yw(x, order.max=100)
fit <- ar(x, method="yw", order.max=100, aic = FALSE)

#Simuloidaan 4001-pituinen signaali
x_sim <- arima.sim(list(ar=fit$ar), sd=sqrt(fit$var.pred), n=4001)
x_sim <- ts(x_sim, frequency=8000, start=0)

ts.plot(x_sim)

#Vertaillaan alkuperäisen signaalin ja simuloidun signaalin ominaisuuksia

par(mfrow=c(2,1))

ts.plot(x)
ts.plot(x_sim)

spec.pgram(x)
spec.pgram(x_sim)

acf(x)
acf(x_sim)

pacf(x)
pacf(x_sim)

#Takaisin äänitiedostoksi

x_sim_scaled <- 30e3*x_sim/max(abs(x_sim))      # "normalisoidaan" y välille [-30000,30000]
f <- file("ah_sim.wav", "wb") # avataan tiedosto, johon simuloitu signaali kirjoitetaan
writeBin(hdr, f)              # kirjoitetaan wav-tiedoston otsikko-osa
writeBin(as.integer(x_sim_scaled), f, size=2) # ...ja itse data
close(f)                      # suljetaan tiedosto
