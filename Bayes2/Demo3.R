#T5

library(rstan)

y <- c(2, 18, 3, 21, 3, 2, 4, 3, 3, 11, 10, 30, 5, 4, 12, 49, 10, 1, 1, 26, 1, 6, 2, 6)

scode5 <- 
"data{
  int<lower=0> N;
  int<lower=0> y[N];
}

parameters{
  real<lower=0> theta[N];
  real<lower=0> phi;
}

model{
  theta ~ exponential(phi);
  for (i in 1:N) y[i] ~ poisson(theta[i]);
}"

fit5 <- stan(
  model_code = scode5,
  data=list(N=length(y), y=y),
  iter=2000
)

s <- summary(fit5, pars=c("phi", "theta[4]", "theta[12]"), probs=c(0.025, 0.975))

s$summary

pairs(fit5, pars=c("theta[4]", "theta[12]"))
plot(fit5, plotfun="hist", pars="phi")

#T6

#1 Aseta alkuarvo theta(t) = theta(0)

#2 Generoi ehdotus theta* ehdotusjakaumasta Tas(theta(t)-a, theta(t)+a)

#3 Laske hyväksymistodennäköisyys normeerattomien posteriorijakaumien avulla

# alpha = min(p(theta*)p(y|theta*) / (p(theta(t))p(y|theta(t))))

#4 Aseta theta(t+1) = theta* tn alpha, muutoin theta(t)

metropolis <- function(n, k, a){
  theta <- rep(NA, n)
  
  theta[1] <- rt(1,k)
  
  for (i in 1:(n-1)){
    theta_c <- runif(1,theta[i]-a, theta[i]+a)
    alpha <- min(1, ((1+(theta_c^2)/k)^(-1/2 * (k+1)))/((1+(theta[i]^2)/k)^(-1/2 * (k+1))))
    theta[i+1] <- ifelse(rbinom(1, 1, alpha) == 1, theta_c, theta[i])
  }
  
  return (theta)
}

#b

library(coda)

iter=4000
x <- 1:iter

thetai <- metropolis(iter, 9, 0.3)
plot(thetai ~ x, type="l")
hist(thetai, breaks=40)

thetaii <- metropolis(iter, 9, 1.5)
plot(thetaii ~ x, type="l")
hist(thetaii, breaks=40)

thetaiii <- metropolis(iter, 9, 2)
plot(thetaiii ~ x, type="l")
hist(thetaiii, breaks=40)

thetaiv <- metropolis(iter, 9, 4)
plot(thetaiv ~ x, type="l")
hist(thetaiv, breaks=40)

autocorr.plot(mcmc(thetai), 50)
autocorr.plot(mcmc(thetaii), 50)
autocorr.plot(mcmc(thetaiii), 50)
autocorr.plot(mcmc(thetaiv), 50)

#iv ketju näyttää kohisevan tasapainotilan ympärillä
#autokorrelaatio näyttää myös vähäiseltä

#c 

mean(thetaiv)

var(thetaiv)
9/(9-2)

#Näyttää estimoituvan melko hyvin
