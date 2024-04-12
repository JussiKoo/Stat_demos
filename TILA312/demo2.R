pisa <- read.table("http://users.jyu.fi/~knordhau/GLM2/pisa.txt", header = TRUE)

mat <- pisa$matem
sp <- as.numeric(pisa$sukup == "tytto")
sij <- as.numeric(pisa$koulusij == "maaseutu")
ita <- as.numeric(pisa$koulualue == "Ita-Suomi")
lansi <- as.numeric(pisa$koulualue == "Lansi-Suomi")
pohjoinen <- as.numeric(pisa$koulualue == "Pohjois-Suomi")
intercept <- rep(1,200)
X <- cbind(intercept, mat, sp, sij, ita, lansi, pohjoinen)
y <- pisa$mpist

fit <- lm(mpist ~ matem + sukup + koulusij + koulualue, data = pisa)
summary(fit)

#Task 2.6
# a. coefficients Beta
# b. Residual standard deviation sigmahat
# c. Standard errors of the estimated coefficients
# d. R^2 of the model

#betahat <- solve(t(X) %*% X) %*% t(X) %*% y #dumb way

#a

betahat <- tcrossprod(solve(crossprod(X)), X) %*% y #better way

#b

epsilonhat <- y - X %*% betahat
n <- length(y)
p <- ncol(X)-1

sigmahat <- sqrt(crossprod(epsilonhat)/(n-p-1))[1,1]
sigmahat

#c

covbeta <-  sigmahat^2 * solve(crossprod(X))

sdbeta <- sqrt(diag(covbeta))
round(sdbeta, 3)

#d

yhat <- y - epsilonhat

SST <- sum((y-mean(y))^2) #total sum of squares
SSR <- sum((yhat-mean(y))^2) #variation explained by the model 

R2 <- SSR/SST
R2 

#===============================================================================

#Task 2.7

#Test the hypothesis H0: Beta2 = 0 with t-test

#Test statistic

t <- betahat[3]/sdbeta[3]

#two tailed t-test p-value

2*pt(t, n-p-1)

#null hypotesis can't be rejected

#===============================================================================

#Task 2.8

#Test with an F-test
#a. H0: Beta1 = ... = Beta6 = 0
#b. H0: Beta4 = Beta5 = Beta6 = 0
#c. H0: Beta4 = Beta6

F_test <- function(K, X, beta, m, sigma)
{
  q <- qr(K)$rank; n <- nrow(X); p <- ncol(X)
  C <- solve(t(K) %*% solve(t(X) %*% X) %*% K)
  F <- t(t(K) %*% beta - m) %*% C %*% (t(K) %*% beta - m) / (q * sigma)
  p_val <- 1 - pf(F, q, n - p - 1)
  list(F = F, p_val = p_val)
}

#a

K <- cbind(c(0,1,0,0,0,0,0),
           c(0,0,1,0,0,0,0),
           c(0,0,0,1,0,0,0),
           c(0,0,0,0,1,0,0),
           c(0,0,0,0,0,1,0),
           c(0,0,0,0,0,0,1))

m <- c(0,0,0,0,0,0)

F_test(K, X, betahat, m, sigmahat^2)

#b

K <- cbind(c(0,0,0,0,1,0,0),
           c(0,0,0,0,0,1,0),
           c(0,0,0,0,0,0,1))

m <- c(0,0,0)

F_test(K, X, betahat, m, sigmahat^2)

#c

K <- cbind(c(0,0,0,0,1,0,-1))

m <- c(0)

F_test(K, X, betahat, m, sigmahat^2)

#===============================================================================

#Task 2.9

#Task 2.9
set.seed(1)
n <- 1000
x1 <- runif(n, -1, 1) #random numbers from uniform distribution
x2 <- rnorm(n, 3, 1.5) #random numbers from normal distribution
x3 <- gl(4,n/4) #factors with values 1,2,3,4 each repeating 250 times
x4 <- gl(2, n/2) #factors with values 1,2 each repeating 500 times
x4 <- sample(x4) #sampling the previous
x5 <- rt(n, df=3) #random numbers from t-distribution with 3 degrees of freedom

x6 <- runif(n,1,4) #random numbers from uniform distribution
x7 <- rnorm(n, 1, 5)
beta1 <- c(2,0.1,0.5,1,0.5,1, -0.5,1.3) #estimate vectors
beta2 <- c(2,0.1,0.5)
beta3 <- c(2,0.1,0.5,0.75)

MM1 <- model.matrix(~x1+x2+x3+x4+x5) #design matrices for different models
MM2 <- model.matrix(~x1+x2)
MM3 <- model.matrix(~x1+x2+I(x2^2))
head(MM1)
head(MM2)
head(MM3)
eps1 <- rnorm(n) #two different vectors of residuals
eps2 <- rnorm(n, 0, x6)
y1 <- MM1 %*% beta1 + eps1 #bunch of different response values simulated
y2 <- exp(MM2 %*% beta2 + eps1)/10
y3 <- MM2 %*% beta2 + eps2
y4 <- MM2 %*% beta2 + eps1
y5 <- MM3 %*% beta3 + eps1
y6 <- MM2 %*% beta2 + 1.5*eps1
y7 <- MM2 %*% beta2 + 2.5*eps1
y8 <- MM2 %*% beta2 + 0.5*eps1
DF <- data.frame(y1, y2, y3, y4, y5, y6, y7, y8, x1, x2, x3, x4, x5, x6, x7)
summary(DF)


#bunch of different linear models are fitted

lm1 <- lm(y1 ~ x1 + x2 + x3 + x4 + x5, data=DF) 

#bunch of different linear models are fitted
lm1 <- lm(y1 ~ x1 + x2 + x3 + x4 + x5, data=DF)
lm2 <- lm(y1 ~ x1 + x2 + x3 + x4, data=DF)
lm3 <- lm(y1 ~ x1 + x2 + x3 + x5 + x7, data=DF)
lm4 <- lm(y2 ~ x1 + x2 , data=DF)
lm5 <- lm(log(y2) ~ x1 + x2 , data=DF)
lm6 <- lm(y3 ~ x1 + x2 , data=DF)
lm7 <- lm(y4 ~ x1 + x2 , data=DF)
lm8 <- lm(y5 ~ x1 + x2 , data=DF)
lm9 <- lm(y5 ~ x1 + x2 + I(x2^2) , data=DF)
lm10 <- lm(y6 ~ x1 + x2 , data=DF)
lm11 <- lm(y7 ~ x1 + x2 , data=DF)
lm12 <- lm(y8 ~ x1 + x2 , data=DF)


#b

#residuals of the models are plotted against certain other variables

#lm1 should be a pretty good fit now. I think the residuals should be
#normally distributed with mean 0 and variance 1.


#b
#residuals of the models are plotted against certain other variables
#lm1 should be a pretty good fit now. I think the residuals should be
#normally distributed with mean 0 and variance 1.
plot(residuals(lm1)~fitted(lm1))
#x1 is uniformly distributed to plot should be very symmetrical all around.
plot(residuals(lm1)~DF$x1)
#x7 is normally distributed with mean 1 and variance 1.5 so it should be seen
#in x-axis.

plot(residuals(lm1)~DF$x7) 

#there's one less explaining variable now so there is probably more variance
plot(residuals(lm2)~fitted(lm2))
plot(residuals(lm2)~DF$x1) #x1 is uniform 

plot(residuals(lm1)~DF$x7)

#there's one less explaining variable now so there is probably more variance
plot(residuals(lm2)~fitted(lm2))
plot(residuals(lm2)~DF$x1) #x1 is uniform
plot(residuals(lm2)~DF$x5) #x5 has nothing to do with this model so can't tell
#x7 is normally distributed with mean 1 and variance 1.5 so it should be seen
#in x-axis.
plot(residuals(lm2)~DF$x7)



#probably similar kind of plot compared to lm1
plot(residuals(lm3)~fitted(lm3))
plot(residuals(lm3)~DF$x1)
plot(residuals(lm3)~DF$x4) #x4 has only two separate values
plot(residuals(lm3)~DF$x7) #x7 is normally distributed so it can be seen in x-axis

#I think that residuals are going to be quite large at some points since y2
#has been exponentially transformed
plot(residuals(lm4)~fitted(lm4))
plot(residuals(lm4)~DF$x1)
plot(residuals(lm4)~DF$x7)

#Taking the logarithm of y2 here should stabilize the scattering of residuals
plot(residuals(lm5)~fitted(lm5))
plot(residuals(lm5)~DF$x1)
plot(residuals(lm5)~DF$x7)

#Wider range for residuals since the epsilon can have variance larger than 1.
plot(residuals(lm6)~fitted(lm6))
plot(residuals(lm6)~DF$x1)
plot(residuals(lm6)~DF$x6) #here you can probably see the different variances

#shorter range for residuals since variance for epsilon is always 1
plot(residuals(lm7)~fitted(lm7))
plot(residuals(lm7)~DF$x2)
plot(residuals(lm7)~DF$x7)

#there's probably pretty big residuals since explaining variables cant explain
#the polynomial term too well
plot(residuals(lm8)~fitted(lm8))
plot(residuals(lm8)~DF$x2) #you can probably see a parabola shape here
plot(residuals(lm8)~DF$x7)

#here the residuals have probably more stable values than in previous model
plot(residuals(lm9)~fitted(lm9))
plot(residuals(lm9)~DF$x2)
plot(residuals(lm9)~DF$x7)

summary(lm7)$adj.r.squared #This value is probably close to 1, maybe 0.8?
summary(lm10)$adj.r.squared #This is probably smaller than previous
summary(lm11)$adj.r.squared #even smaller
summary(lm12)$adj.r.squared #biggest of these, let's say 0.4

beta2
coef(lm7) #Let's guess 1.6, 0.15, 0.44 
coef(lm10) #These are more off than previous: 2.15, 0.25, 0.49
coef(lm11) #even more off: 2.19, 0.3, 0.5
coef(lm12) #These should be pretty close: 2.03, 0.09, 0.47

coef(lm7) #Let's guess 1.6, 0.15, 0.44
coef(lm10) #These are more off than previous: 2.15, 0.25, 0.49
coef(lm11) #even more off: 2.19, 0.3, 0.5
coef(lm12) #These should be pretty close: 2.03, 0.09, 0.47
