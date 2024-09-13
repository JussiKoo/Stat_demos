#T6

#b) Kasvattamalla otoskokoa posteriorit ovat paljon lähempänä toisiaan

y <- 5
n <- 16

alpha <- 0.01
beta <- 0.01

#Priori Beta(0.01, 0.01)
#D2 T3 johdettiin vastaava posteriorijakauma Beta(alpha + y, beta + n -y)

curve(dbeta(x, alpha + y, beta + n - y), xlab = expression(theta), ylab = "density", lwd = 2, ylim=c(0,40))

#Priori Beta(1,1)

alpha <- 1
beta <- 1

curve(dbeta(x, alpha + y, beta + n - y), xlab = expression(theta), ylab = "density", lwd = 2, add=TRUE, col="red")

#Priori Beta(100,100)

alpha <- 100
beta <- 100

curve(dbeta(x, alpha + y, beta + n - y), xlab = expression(theta), ylab = "density", lwd = 2, add=TRUE, col="blue")

alpha <- 10
beta <- 1

curve(dbeta(x, alpha + y, beta + n - y), xlab = expression(theta), ylab = "density", lwd = 2, add=TRUE, col="green")

legend(0, 12, legend=c("Beta(0.01, 0.01)", "Beta(1,1)", "Beta(100, 100)", "Beta(10,1)"), col=c("black", "red", "blue", "green"), lty=1)

#T7

library(brms)

fit1 <- brm(
  count ~ zBase * Trt + (1|patient),
  data = epilepsy,
  family = poisson(),
  prior = prior(normal(0, 10), class = b) +
    prior(cauchy(0, 2), class = sd)
)

fit1

#Trt posteriorikeskiarvo -0.29
#95% posterioriväli [-0.61. 0.03]