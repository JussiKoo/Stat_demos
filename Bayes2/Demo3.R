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
