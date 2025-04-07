#T1

#a

data_eq <- read.csv2("eq.csv", header=TRUE, sep = ",")

eq_ts <- ts(data_eq$Quakes, start=1900, freq=1)

#b

#alkuperäinen

ts.plot(eq_ts)

acf(eq_ts, lag.max = 50)

pacf(eq_ts, lag.max = 50) #AR(1)?

#erotussarja

diff_eq_ts <- diff(eq_ts)

ts.plot(diff_eq_ts)

acf(diff_eq_ts, lag.max = 50, ci.type="ma") #MA(1)?

pacf(diff_eq_ts, lag.max = 50)

#c

#MA(1) mallia voisi sovittaa sillä näyttää että otosautokorrelaatiot ovat
#lähellä nollaa viiveestä 1 eteenpäin.

#AR(1) mallia voisi sovittaa sillä näyttää, että osittaiset autokorrelaatiot
#ovat lähellä nollaa viiveestä 1 eteenpäin.

model_ma1 <- arima(diff_eq_ts, order=c(0,0,1))
model_ma1

model_ar1 <- arima(eq_ts, order=c(1,0,0))

#d

confint(model_ma1)

#e

model_ma1$var.coef

#T2

#standard error

se <- sqrt(model_ma1$var.coef[1])

#confint

model_ma1$coef[1] + qnorm(0.975)*se
model_ma1$coef[1] - qnorm(0.975)*se

#===============================================================================

#T2

#a

data(AirPassengers)

data_ap <- ts(log(AirPassengers), start=1949, frequency = 12)
data_ap

ts.plot(data_ap)

#b

months_ <- month.abb[cycle(AirPassengers)]

t <- as.numeric(time(data_ap))

fit_b <- lm(data_ap ~ months_ + t)

#c

ts.plot(data_ap, fitted(fit_b), gpars=list(xlab="vuosi", ylab="airpassengers", 
                                           col=c("black","red")))

ts.plot(data_ap - fitted(fit_b))

#d

#Residuaalit eivät näytä stationaarisilta. Välillä (esim 1954-1956) on
#havaittavissa kasvavaa lineaarista trendiä ja välillä laskevaa lineaarista
#trendiä.

#e

fit_e <- arima(data_ap, order=c(0,1,0), seasonal=c(2,0,0), xreg=t)

fit_e$model$phi

sim_e <- arima.sim(model = list(ar=fit_e$model$phi), length(data_ap), start)

ts.plot(diff(data_ap))

ts.plot(sim_e)

ts.plot(data_ap, sim_e, gpars=list(xlab="vuosi", ylab="residual", 
                                                            col=c("black","red")))

#f





