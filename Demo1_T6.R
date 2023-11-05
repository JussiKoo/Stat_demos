#function to evaluate likelihood.
L <- function(theta, n, k) {
  choose(n,k)*theta^k*(1-theta)^(n-k)
}

#function that draws the curve
drawLfunction <- function(file) {
  phone_talking <- read.csv(file)
  
  #n is number of observations.
  #k is the number of occurences.
  n <- length(phone_talking$obs)
  k <- length(which(phone_talking$talking == 1))
  
  curve(L(x,n,k), from=0, to=1, xlab="theta", ylab="Likelihood", lwd=2)
}

drawLfunction("C:\\kurssit\\TILA141\\phone_talking.csv")

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






