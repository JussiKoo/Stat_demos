# Excercise 2

p = 1-sum(dpois(0:9, 7.5))
p

# Excercise 5

x <- c(0.09, 0.18, 0.58, 0.12, 0.24, 1.27, 0, 0.01, 0.62, 0.62, 0.18,
       0.16, 0.09, 1.26, 0.33, 0.22, 0.23, 0.2, 0.1, 0.02, 0.41, 0.67,
       0.88, 0.38, 0.18, 1.17, 0.01, 0.05, 0.21, 0.16)

#End when distance between theta1 and theta2 is less or equal to this
end <- 0.001

#Starting value
theta1 <- 5

#Initializing other necessary parameters
theta2 <- 0
sx <- sum(x)
n <- length(x)
d <- 1000

while (d > end) {
  theta2 <- 2*theta1 - ((theta1^2)/n)*sx
  d <- abs(theta2-theta1)
  theta1 <- theta2
}

theta2

# Excercise 6

neglogL <- function(x) {
  -n*log(x)+x*sx
}

dneglogL <- function(x) {
  -n/x+sx
}

optim(par=10, neglogL, gr = NULL,
      method = "L-BFGS-B",
      lower = -10, upper = 10, hessian = FALSE)

optim(par=10, neglogL, gr = dneglogL,
      method = "L-BFGS-B",
      lower = -10, upper = 10, hessian = FALSE)



