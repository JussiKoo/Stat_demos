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

F_test(K, X, betahat, m, sigma2hat)

#b

K <- cbind(c(0,0,0,0,1,0,0),
           c(0,0,0,0,0,1,0),
           c(0,0,0,0,0,0,1))

m <- c(0,0,0)

F_test(K, X, betahat, m, sigma2hat)

#c



