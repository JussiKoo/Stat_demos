#R5

library(statmod)
library(MASS)

ants <- read.table("http://users.jyu.fi/~knordhau/GLM2/ants.txt")
ants[1:10,]

attach(ants)

fit1 <- glm(counts ~ bg + sc + fmd, data=ants, family=poisson)
summary(fit1)

fit2 <- glm(counts ~ bg * sc + fmd, data=ants, family=poisson)
anova(fit2, fit1, test="Chisq")

b <- exp(coef(fit1))
b

exp(confint(fit1))

ex <- data.frame(sc = rep(0, 51), fmd = rep(0,51), bg = seq(0,50,1))

p1 <- predict(fit1, ex, type="response", se.fit=TRUE)

plot(seq(0,50,), p1$fit, type="l", ylim=c(0,10))
lines(seq(0,50), p1$fit - 1.96*p1$se.fit, type="l")
lines(seq(0,50), p1$fit + 1.96*p1$se.fit, type="l")

fc <- b[1] + b[2]*ex$bg + b[3]*ex$sc + b[4]*ex$fmd

#Dunn-Smyth

plot(qresiduals(fit1) ~ fitted(fit1))

fit3 <- glm(counts ~ bg + sc + fmd, data=ants, family=quasipoisson)
summary(fit3)
summary(fit1)

fit4 <- glm.nb(counts ~ bg + sc + fmd, data=ants)
summary(fit4)
summary(fit3)

plot(qresiduals(fit4) ~ fitted(fit4))

#Doctors data

heart <- read.table("http://users.jyu.fi/~knordhau/GLM2/heart.txt")
heart$agecat <- c(1:5,1:5)
heart[1:10,]
attach(heart)

library(ggplot2)
ggplot(data=heart, aes(x=age, y=deaths/pyears * 1e5))+
  geom_point(aes(shape = factor(smoke)))+
  theme_classic()

fith <- glm(deaths ~ agecat * smoke + offset(log(pyears)),
            data=heart, family=poisson)
summary(fith)
