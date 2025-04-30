#T1

f <- file("ah.wav", "rb")    # avataan tiedosto luettavaksi
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
writeBin(as.integer(x_sim), f, size=2) # ...ja itse data
close(f)                      # suljetaan tiedosto

#===============================================================================

#T5

#===============================================================================

#m_j|k = E[X_j | y[1:k]
#P_j|k = Var(X_j | y[1:k])

#m_1|0 = F*m_0|0

#m_1|1 = m_1|0 + P_1|0

kf <- function(y, m0=0, P0=1, FF=1, Q=1, H=1, R=1) {
  # Kirjoita oma toteutuksesi tähän
  
  n <- length(y)
  
  m <- rep(NA, n+1)
  P <- rep(NA, n+1)
  
  m_1step <- rep(NA, n)
  P_1step <- rep(NA, n)
  
  m[1] <- m0
  P[1] <- P0
  
  z <- rep(NA, n)
  S <- rep(NA, n)
  
  L <- 0
  
  for (i in 1:n) {
    m_1step[i] <- FF*m[i]
    P_1step[i] <- FF**2 * P[i] + Q
    
    z[i] <- y[i] - H * m_1step[i]
    S[i] <- H**2 * P_1step[i] + R
    
    m[i+1] <- m_1step[i] + P_1step[i] * H / S[i] * z[i]
    P[i+1] <- (1 - P_1step[i] * H**2 / S[i]) * P_1step[i]
    
    L <- L + dnorm(z[i], mean=0, sd=sqrt(S[i]), log=TRUE)
  }
  
  result = list(m=tail(m,1), P=tail(P,1), L=L)
  
  return(result)
}

set.seed(1234)
n <- 200; sigma_0 <- 1; sigma_mu <- 0.2; sigma_y <- 1
y <- mu <- rep(0,n)
mu_ <- rnorm(1, sd=sigma_0)
for (k in 1:n) {
  mu_ = mu_ + rnorm(1, sd=sigma_mu)
  mu[k] <- mu_; y[k] <- mu_ + rnorm(1, sd=sigma_y)
}

kf(y, Q=sigma_mu**2, R=sigma_y**2)
