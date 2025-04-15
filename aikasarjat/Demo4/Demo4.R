#T1

#a

empl <- read.csv2("empl.csv", header=TRUE)

osuus <- empl$unempl/empl$population

ue <- ts(osuus, start=1993, frequency=12)

ts.plot(ue)

ts.plot(diff(ue))

#Ennemmin erotussarjaa voisi mallintaa stationaarisella ARMA-mallilla.

#Tutkitaan vielä autokorrelaatioita erotussarjalle

acf(diff(ue), lag.max = 50)

pacf(diff(ue))

#b

diff_ue <- ts(diff(ue), start=1993, frequency = 12)

arima_p <- function(p, P) {
  return (arima(diff_ue, order=c(p,1,0), seasonal=c(P,0,0)))
}

#P*s + p 

arima20 <- arima_p(2, 0) #2 parametria

arima21 <- arima_p(2, 1) #5 parametria

arima32 <- arima_p(3, 2) #11 parametria

n <- length(diff_ue)

#tavallinen AIC

AIC(arima20, arima21, arima32)

#korjattu AIC

AIC(arima20, k=2*n/(n-4+1))

AIC(arima21, k=2*n/(n-7+1))

AIC(arima32, k=2*n/(n-13+1))

#BIC

AIC(arima20, k=log(n))

AIC(arima21, k=log(n))

AIC(arima32, k=log(n))

#d

predict_plot <- function(mod, h, newxreg) {
  p <- predict(mod, h, newxreg=newxreg)
  m <- p$pred; s <- p$se
  ts.plot(b, m, m+1.96*s, m-1.96*s, col=c(1,2,2,2), lty=c(1,1,2,2))
}

h <- 48 

t <- time(diff_ue)

nx <- t[length(t)] + (1:h)/12

predict_plot(arima32, h, nx)

#===============================================================================

#T2