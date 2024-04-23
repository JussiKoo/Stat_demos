#1

pisa <- read.table("http://users.jyu.fi/~knordhau/GLM2/pisafull.txt", header = TRUE)

#cut-function could be used to categorise in multiple levels
pisa$SC650 <- 1 * (pisa$sciescore > 650)
pisa$Math8 <- pisa$matem - 8

attach(pisa)

#2

fit <- glm(SC650 ~ Math8 * motiv + gender + ESCS, data=pisa, family=binomial)

summary(fit)

beta <- coef(fit)

#3 and 4

invlogit <- function(x) plogis(x)

invlogit(beta[1])

invlogit(beta[1] + beta[3])

invlogit(beta[1] + beta[4])

invlogit(beta[1] + beta[3] + beta[4])

invlogit(beta[1] + beta[2] + beta[3] + beta[5] + beta[6])

invlogit(beta[1] + beta[2] + beta[3] + beta[4] + beta[5] + beta[6])

#5 6 7

mat <- seq(4,10, by=0.1)

plot(mat, invlogit(beta[1] + beta[2] * (mat-8)), type="l", col="red")
lines(mat, invlogit(beta[1] + beta[3] + (beta[2] + beta[6] )* (mat-8)), col="blue")

plot(mat, invlogit(beta[1] + beta[3] + (beta[2] + beta[6] )*(mat-8)), type="l", col="red")
lines(mat, invlogit(beta[1] + beta[3] + (beta[2] + beta[6] )* (mat-8)), col="blue")

escs <- seq(-3.2, 2.7, by=0.1)

plot(escs, invlogit(beta[1] + beta[5] * escs) , type="l", col="red")
lines(escs, invlogit(beta[1] + beta[4] + beta[5]*escs ), col="blue")

#8

exp(beta[1])

#9

exp(beta[5])

exp(beta[4])

exp(beta[3])

exp(beta[2])

exp(beta[2] + beta[6])

#10

fit2 <- glm(SC650 ~ Math8 * motiv + gender + ESCS, data=pisa, family=binomial(link="probit"))

summary(fit2)

#1 

w <- read.table("http://users.jyu.fi/~knordhau/GLM2/womensrole.txt", header = TRUE)

w$sex <- as.factor(w$sex)

y <- w$agree / (w$agree + w$disagree)

plot(y ~ w$education, data=w, col = as.numeric(w$sex))

fit <- glm(cbind(agree, disagree) ~ sex * education, data = wr, family = binomial())

r <- residuals(fit, type = "pearson")
sum(r^2)
qchisq(0.95, 37)
plot(r ~ fitted(fit))


