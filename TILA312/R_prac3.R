scoreN <- function(y, mu, n){
  n*(mean(y) - mu)
}

score_norm <- function(n){
  mu <- seq(0,6,0.1)
  y <- rnorm(n,4,1)
  plot(mu, scoreN(y,mu,n), type="l", xlab="")
  
  for(i in 1:19){
    y <- rnorm(n,4,1)
    lines(mu, scoreN(y, mu, n), type="l")
  }
  abline(0,0)
}

score_norm(10000)

scoreB<- function(y, pi, n){
  n*(mean(y)-pi)/(pi*(1-pi))
}

score_bern <- function(n){
  pi <- seq(0.1,0.8,0.01)
  y <- rbinom(n,1, 0.4)
  plot(pi, scoreB(y,pi,n), type="l", xlab="")
  
  for(i in 1:19){
    y <- rbinom(n,1, 0.4)
    lines(pi, scoreB(y, pi, n), type="l")
  }
  abline(0,0)
}

score_bern(100)


scoreP<- function(y, lambda, n){
  n*(mean(y)-lambda)/lambda
}

score_pois <- function(n){
  lambda <- seq(2,6,0.1)
  y <- rpois(n, 4)
  plot(lambda, scoreP(y,lambda,n), type="l", xlab="")
  
  for(i in 1:19){
    y <- rpois(n, 4)
    lines(lambda, scoreP(y, lambda, n), type="l")
  }
  abline(0,0)
}

score_pois(10)

scores <- numeric(100)

for (i in 1:100) {
  x <- rpois(100,4)
  scores[i] <- scoreP(x,4,100)
}
plot(density(scores))

#=============================

cod <- read.table("http://users.jyu.fi/~knordhau/GLM2/fish.txt", header = TRUE)
nrow(cod)

cod <- na.omit(cod)
nrow(cod)

cod$Year <- factor(cod$Year)
is.factor(cod$Year)
cod$Area <- factor(cod$Area)

#test correlations between predictors
boxplot(cod$Depth ~ cod$Area)
plot(cod$Length ~ cod$Weight)

fit1 <- glm(cod$Prevalence ~ cod$Length + cod$Year + cod$Area, family="binomial")
summary(fit1)

fit2 <- glm(cod$Prevalence ~ cod$Length + cod$Year * cod$Area, family="binomial")

D <- deviance(fit1) - deviance(fit2)

#6 degrees of freedom

#chi^2 

fit3 <- glm(cod$Prevalence ~ cod$Year * cod$Area, family="binomial")

summary(fit3)



