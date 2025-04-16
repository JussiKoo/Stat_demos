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

p <- predict(arima32, n.ahead=48)
m <- p$pred; s <- p$se
ts.plot(diff_ue, m, m+1.96*s, m-1.96*s, col=c(1,2,2,2), lty=c(1,1,2,2), 
        xlim=c(2009, 2020))

#===============================================================================

#T2

funcc <- function(model, p, q) {
  par(mfrow=c(2,1))
  ts.plot(model$residuals)
  res_acf <- acf(model$residuals)
  
  n <- length(model$residuals)
  
  pval <- rep(NA,20)
  
  for (lag in (p+q+1):20) {
    Q <- n*sum((res_acf$acf[1:lag] ** 2) * (n+2)/(n-(1:lag)))
    pval[lag] <- 1-pchisq(Q, df=lag-p-q)
  }
  print(pval)
  
  #Q <- n*cumsum((n+2)/(n+(1:20)) * res_acf$acf[1:20]**2)
  #Q <- tail(Q, p+q)
  
  #p <- 1-pchisq(Q, df=(p+q:20)-3)
  #print(p)
}

set.seed(12345)
x <- arima.sim(model=list(ar=c(1/2, 1/3)), 80)
fit <- arima(x, order=c(0,0,3))

tsdiag(fit)

funcc(fit, 0, 3)
