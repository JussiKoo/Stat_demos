#T2

library(mclust)

set.seed(20190125)
x <- c(rnorm(500, 179, 8), rnorm(1000, 172, 6))

#a

#Mallin parametrit
mean(x)
sd(x)

#b

mixture_model <- Mclust(x)
print(summary(mixm,parameters=TRUE))

#c

xsorted <- sort(x)

plot(mixm, "density", col ="red")
lines(density(x,bw="SJ"))
lines(xs,dnorm(xs,mean(xs),sd(xs)), col="blue")

#T6

set.seed(20190204)
theta <- 3
n <- 200
Z <- 1 + rbinom(n, 1, 0.7)
x <- (Z==1) * rpois(n,1) + (Z==2) * rpois(n,3)
rm(Z)

logL <- function(theta){
  return(sum(log(0.3*dpois(x, 1) + 0.7*dpois(x, theta))))
}

optimize(f=logL, interval=c(0,4), maximum=TRUE)
