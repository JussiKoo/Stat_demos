#T1

library(rstan)
library(coda)



#a

url <- "http://users.jyu.fi/~santikka/bayes2/data/sideeffect.dat"
sideeffects <- read.table(url, header = TRUE)

scode1 <-
  "data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0> y[N];
  int<lower=0> n[N];
  real xtilde;
  real ytilde;
}
parameters {
  real alpha;
  real beta;
}
model {
  alpha ~ normal(1, 10);
  beta ~ normal(1, 10);
  y ~ binomial_logit(n, alpha + beta * x);
}
generated quantities {
  real pred_y = binomial_rng(1, inv_logit(alpha + beta * xtilde));
  real pred_x = (logit(ytilde)-alpha)/beta;
}"

#(logit(0.5)-a)/b = x


fit1 <- stan(
  model_code = scode1,
  data=list(
    N=nrow(sideeffects),
    x=sideeffects$annostus,
    n=sideeffects$potilaita,
    y=sideeffects$sivuvaik.,
    xtilde=1.5,
    ytilde=0.5),
  iter=2000
)

summary(fit1)

params <- extract(fit1)

plot(fit1, plotfun="hist", pars=c("alpha", "beta"))

#b

plot(fit1, plotfun="hist", pars=c("pred_x"))
s <- summary(fit1, pars=c("pred_x"))

s$summary

#c

mean(params$pred_y)

#T2

y <- c(
  3.24, 1.42, 6.83, 3.60, 4.30, 0.81, 2.83, 1.01, 3.23, 5.99, 2.70, 2.09,
  4.88, 5.31, 0.44, 1.98, 0.19, 4.26, 2.39, 2.04, 1.29, 1.01, 2.57, 0.03
)

scode2 <-
  "data {
  int<lower=0> n;
  real<lower=0> y[n];
}
parameters {
  real<lower=0> theta;
}
model {
  theta ~ inv_gamma(4, 3/2);
  y ~ exponential(theta);
}
generated quantities{
  real<lower=0> y_tilde = exponential_rng(theta);
}"

fit2 <- stan(
  model_code = scode2,
  data=list(
    y=y,
    n=length(y)
  ),
  iter=4000
)

summary(fit2)$summary
params2 <- extract(fit2)

mean(params2$y_tilde > 6)

#T3

y <- c(
  3.24, 1.42, NA, 3.60, 4.30, 0.81, 2.83, 1.01, 3.23, 5.99, 2.70, 2.09,
  4.88, 5.31, 0.44, 1.98, 0.19, 4.26, 2.39, 2.04, 1.29, 1.01, 2.57, 0.03
)

#Kyseessä on sensurointi jossa raja on siis jäljelle jääneistä arvoista suurin.

y_obs <- y[!is.na(y)]
y_cens <- y[is.na(y)]

scode3 <- 
"data {
  int<lower=0> N_obs;
  int<lower=0> N_cens;
  real y_obs[N_obs];
  real<lower=max(y_obs)> U;
}
parameters {
  real<lower=U> y_cens[N_cens];
  real<lower=0> theta;
}
model {
  theta ~ inv_gamma(4, 3/2);
  y_obs ~ exponential(theta);
  y_cens ~ exponential(theta);
}"

fit3 <- stan(
  model_code = scode3,
  data=list(
    N_obs = length(y_obs),
    N_cens = length(y_cens),
    y_obs = y_obs,
    U = max(y_obs)
  ),
  iter = 4000
)

s <- summary(fit3)
s$summary

plot(fit3, pars=c("theta"), plotfun="hist")
plot(fit3, pars=c("y_cens"), plotfun="hist")

#T4

y <- c(2, 18, 3, 21, 3, 2, 4, 3, 3, 11, 10, 30, 5, 4, 12, 49, 10, 1, 1, 26, 1, 6, 2, 6)

scode4 <- 
"data {
  int<lower=0> N;
  int<lower=0> y[N];
}
parameters {
  real<lower=0> theta[N];
  real<lower=0> phi;
}
model {
  theta ~ exponential(phi);
  y ~ poisson(theta);
}"

fit4 <- stan(
  model_code = scode4,
  data = list(
    N = length(y),
    y = y
  ),
  iter = 2000,
  chains = 4
)

fit4

params4 <- rstan::extract(fit4)
params4$phi2 <- params4$phi^2
phi_chains <- list()
phi_chains[[1]] <- as.matrix(params4$phi[1:1000])
phi_chains[[2]] <- as.matrix(params4$phi[1001:2000])
phi_chains[[3]] <- as.matrix(params4$phi[2001:3000])
phi_chains[[4]] <- as.matrix(params4$phi[3001:4000])
phi_mcmc <- as.mcmc.list(lapply(phi_chains, mcmc))

coda::effectiveSize(phi_mcmc)

phi2_chains <- list()
phi2_chains[[1]] <- as.matrix(params4$phi2[1:1000])
phi2_chains[[2]] <- as.matrix(params4$phi2[1001:2000])
phi2_chains[[3]] <- as.matrix(params4$phi2[2001:3000])
phi2_chains[[4]] <- as.matrix(params4$phi2[3001:4000])
phi2_mcmc <- as.mcmc.list(lapply(phi2_chains, mcmc))

coda::effectiveSize(phi2_mcmc)

thetaess <- rep(NA, 24)

for (i in 1:24){
  theta_chains <- list()
  theta_chains[[1]] <- as.matrix(params4$theta[1:1000,i])
  theta_chains[[2]] <- as.matrix(params4$theta[1001:2000,i])
  theta_chains[[3]] <- as.matrix(params4$theta[2001:3000,i])
  theta_chains[[4]] <- as.matrix(params4$theta[3001:4000,i])
  theta_mcmc <- as.mcmc.list(lapply(theta_chains, mcmc))
  
  thetaess[i] <- coda::effectiveSize(theta_mcmc)
}

thetaess


#Ensin posteriorinäytteisiin muunnos ja sitten lasketaan tehokkaat otoskoot.

#T5

y <- c(
  2.17, 4.04, 3.58, 2.42, 3.34, 1.89, 2.15, 2.39, 2.62, 1.96, 2.13, 2.61,
  1.90, 1.96, 1.81, 2.33, 2.25, 3.71, 1.78, 1.99, 2.71, 6.71, 2.36, 4.18
)

#T6

library("foreign")
url <- "http://users.jyu.fi/~santikka/bayes2/data/pisa2009FINs.sav"
pisadata <- read.spss(url, to.data.frame = TRUE)