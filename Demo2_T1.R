#Excercise 1

phone_talking <- read.csv("C:\\kurssit\\TILA141\\phone_talking.csv")

#function to evaluate likelihood.
L <- function(theta, n, k) {
  choose(n,k)*theta^k*(1-theta)^(n-k)
}

n1 <- 50
n2 <- 100
n3 <- 200

k1 <- sum(phone_talking$talking[1:50])
k2 <- sum(phone_talking$talking[1:100])
k3 <- sum(phone_talking$talking)

m1 <- max(L(seq(0.1, 1, by=0.01), n1, k1))
m2 <- max(L(seq(0.1, 1, by=0.01), n2, k2))
m3 <- max(L(seq(0.1, 1, by=0.01), n3, k3))

curve(L(x,n1,k1)/m1, from=0, to=1, xlab="theta", ylab="Likelihood", lwd=2, col="green")
curve(L(x,n2,k2)/m2, from=0, to=1, xlab="theta", ylab="Likelihood", lwd=2, col="red", add=TRUE)
curve(L(x,n3,k3)/m3, from=0, to=1, xlab="theta", ylab="Likelihood", lwd=2, col="blue", add=TRUE)

#===============================================================================

#Excercise 2

#a) Likelihood-function

X <- c(8,9,15,16,19,21,44,51,57)

curve(x^9*exp(-240*x), from=0, to=1, xlab="theta", ylab="likelihood", lwd=2)

#b) Log-likelihood function

curve(9*log(x)-240*x, from=0, to=1, xlab="theta", ylab="likelihood", lwd=2)

#c) score-function

curve(9/x-240, from=0, to=1, xlab="theta", ylab="likelihood", lwd=2)

#d)

curve(-9/x^2, from=0, to=1, xlab="theta", ylab="likelihood", lwd=2)

#===============================================================================

#Excercise 6

#a)

n <- 1000

x1 <- rnorm(n, mean=0, sd=1)
x1

m1 <- mean(x1)
m2 <- mean(x1^2)
m3 <- mean(x1^3)
m4 <- mean(x1^4)

means1 <- c()
means2 <- c()
means3 <- c()
means4 <- c()

for (i in 1:n) {
  means1 <- append(means1, mean(x1[1:i]))
  means2 <- append(means2, mean(x1[1:i]^2))
  means3 <- append(means3, mean(x1[1:i]^3))
  means4 <- append(means4, mean(x1[1:i]^4))
}

plot(1:n, means1)
abline(a=m1,b=0)

plot(1:n, means2)
abline(a=m2,b=0)

plot(1:n, means3)
abline(a=m3,b=0)

plot(1:n, means4)
abline(a=m4,b=0)

#b) 

x2 <- rt(n, df=4)
x2

m1 <- mean(x2)
m2 <- mean(x2^2)
m3 <- mean(x2^3)
m4 <- mean(x2^4)

means1 <- c()
means2 <- c()
means3 <- c()
means4 <- c()

for (i in 1:n) {
  means1 <- append(means1, mean(x2[1:i]))
  means2 <- append(means2, mean(x2[1:i]^2))
  means3 <- append(means3, mean(x2[1:i]^3))
  means4 <- append(means4, mean(x2[1:i]^4))
}

plot(1:n, means1)
abline(a=m1,b=0)

plot(1:n, means2)
abline(a=m2,b=0)

plot(1:n, means3)
abline(a=m3,b=0)

plot(1:n, means4)
abline(a=m4,b=0)

#c)

x3 <- rcauchy(n, location=0, scale=1)
x3

m1 <- mean(x3)
m2 <- mean(x3^2)
m3 <- mean(x3^3)
m4 <- mean(x3^4)

means1 <- c()
means2 <- c()
means3 <- c()
means4 <- c()

for (i in 1:n) {
  means1 <- append(means1, mean(x3[1:i]))
  means2 <- append(means2, mean(x3[1:i]^2))
  means3 <- append(means3, mean(x3[1:i]^3))
  means4 <- append(means4, mean(x3[1:i]^4))
}

plot(1:n, means1)
abline(a=m1,b=0)

plot(1:n, means2)
abline(a=m2,b=0)

plot(1:n, means3)
abline(a=m3,b=0)

plot(1:n, means4)
abline(a=m4,b=0)
