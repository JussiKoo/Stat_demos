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




