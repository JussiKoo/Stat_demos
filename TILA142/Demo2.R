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

