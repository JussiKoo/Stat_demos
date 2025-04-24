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

#Sovitetaan joitain malleja

arima10 <- arima_p(1, 0) #1 parametri

arima20 <- arima_p(2, 0) #2 parametria

arima21 <- arima_p(2, 1) #5 parametria

arima32 <- arima_p(3, 2) #11 parametria

n <- length(diff_ue)

#tavallinen AIC

AIC(arima10, arima20, arima21, arima32)

#korjattu AIC

AIC(arima10, k=2*n/(n-3+1))

AIC(arima20, k=2*n/(n-4+1))

AIC(arima21, k=2*n/(n-7+1))

AIC(arima32, k=2*n/(n-13+1))

#BIC

AIC(arima10, k=log(n))

AIC(arima20, k=log(n))

AIC(arima21, k=log(n))

AIC(arima32, k=log(n))

#d

p <- predict(arima32, n.ahead=48)
m <- p$pred; s <- p$se
ts.plot(diff_ue, m, m+1.96*s, m-1.96*s, col=c(1,2,2,2), lty=c(1,1,2,2), 
        xlim=c(2009, 2020))

#===============================================================================

#T2

my_diagnosis <- function(model, p, q) {
  par(mfrow=c(2,2))
  ts.plot(model$residuals)
  res_acf <- acf(model$residuals)
  
  n <- length(model$residuals)
  
  pval <- rep(NA,10)
  
  #Lasketaan Ljung-Box tunnuslukuja eri K arvoilla (p+q < K << n)
  for (K in (p+q+1):(p+q+10)) {
    Q <- n*sum((res_acf$acf[2:(K+1)]^2) * (n+2)/(n-(2:(K+1))))
    pval[K - p - q] <- 1-pchisq(Q, df=K-p-q)
  }
  plot(x=(p+q+1):(p+q+10), y=pval, xlab="K")
  
  #Q <- n*cumsum((n+2)/(n+(1:20)) * res_acf$acf[1:20]**2)
  #Q <- tail(Q, p+q)
  
  #p <- 1-pchisq(Q, df=(p+q:20)-3)
  #print(p)
}

set.seed(12345)
x <- arima.sim(model=list(ar=c(1/2, 1/3)), 80)
fit <- arima(x, order=c(0,0,3))

tsdiag(fit)

my_diagnosis(fit, 0, 3)
