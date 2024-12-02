#T1

hiili <- scan("http://users.jyu.fi/~santikka/bayes2/data/coal.dat")

disaster_gibbs <- function(y, n_sim, n_burn, theta_0, A, B, C, D) {
  n <- length(y)
  # Tässä theta = (lambda, mu, k)
  theta <- matrix(ncol = 3, nrow = n_sim + n_burn)
  theta[1, ] <- theta_0
  k <- theta[1, 3]
  for (i in 2:(n_sim + n_burn)) {
    lambda <- rgamma(1, A + sum(y[1:k]), rate = B + k)
    mu <- rgamma(1, C + sum(y[(k + 1):n]), rate = D + (n - k))
    k_star <- sample((k - 1):(k + 1), size = 1)
    if (k_star == 0) {
      k_star <- n - 1
    }
    else if (k_star == n) {
      k_star <- 1
    }
    r <- lambda^(sum(y[1:k_star]) - sum(y[1:k])) *
      mu^(sum(y[(k_star + 1):n]) - sum(y[(k + 1):n])) *
      exp(-(k_star - k) * lambda + (k_star - k) * mu)
    k <- ifelse(rbinom(1, size = 1, prob = min(1, r)), k_star, k)
    theta[i,] <- c(lambda,mu,k)
  }
  theta[seq(n_burn + 1, n_burn + n_sim), ]
}
dis_res <- disaster_gibbs(hiili, 2e4, 1e4, c(4, 1, 40), 0.01, 0.01, 0.01, 0.01)
dis_res[, 3] <- dis_res[, 3] + 1850 # muunnetaan erotus vuodesta 1850 vuosiluvuksi

#T2

library(rstan)
library(tidyr)

url <- "http://users.jyu.fi/~santikka/bayes2/data/Margus_etal_data_gen_2.csv"
beetle <- read.csv2(url)
head(beetle)

#Vasteet Weight_7d_mg ja Ad_weight_mg

#Mallinnus moniulotteisella normaalijakaumalla

#odotusarvovektoriin selittäjinä Sex ja Treatment

scode <- "
data {
  int<lower=0> n;         // mittausten määrä
  int<lower=0, upper=1> sex[n]; // kuoriaisen sukupuoli
  int<lower=0, upper=1> treatment[n]; //käsittely
  real weight_7d[n];
  real adultweight[n];
}
transformed data {
  vector[2] y[n];
  vector[2] x[n];
  
  for (i in 1:n) {
    y[i, 1] = weight_7d[i];
    y[i, 2] = adultweight[i];
    x[i, 1] = sex[i];
    x[i, 2] = treatment[i];
  }
}
parameters {
  vector[2] beta[2];
  cov_matrix[2] Sigma;
}
transformed parameters {
  vector[2] mu[n];
  for (i in 1:n) {
    mu[i, 1] = mean(y[,1]) + beta[1, 1]*x[i, 1] + beta[1, 2]*x[i, 2];
    mu[i, 2] = mean(y[,2]) + beta[2, 1]*x[i, 1] + beta[2, 2]*x[i, 2];
  }
}
model {
  Sigma ~ inv_wishart(2, diag_matrix(rep_vector(100,2)));
  y ~ multi_normal(mu, Sigma);
  beta[1,] ~ normal(0,100);
  beta[2,] ~ normal(0,100);
}
"

beetle$Treatment <- beetle$Treatment-1
beetle$Sex <- beetle$Sex-1 
beetle$Ad_weight_mg_scaled <- scale(beetle$Ad_weight_mg)
beetle$Weight_7d_mg_scaled <- scale(beetle$Weight_7d_mg)
beetle_na <- drop_na(beetle)

fit2 <- stan(
  model_code = scode,
  data = list(
    n=nrow(beetle_na),
    sex = beetle_na$Sex,
    treatment = beetle_na$Treatment,
    weight_7d = beetle_na$Weight_7d_mg_scaled[,1],
    adultweight = beetle_na$Ad_weight_mg_scaled[,1]
    ),
  iter=2000,
  chains = 4
)

#beta[1,1] sukupuolen vaikutus varhaispainoon
#beta[1,2] käsittelyn vaikutus varhaispainoon
#beta[2,1] sukupuolen vaikutus aikuispainoon
#beta[2,2] käsittelyn vaikutus aikuispainoon
summary(fit2, pars=c("beta"), probs=c(0.025, 0.975))

#T3

scode3 <- "
data{
  int<lower=0> y[4];
}
parameters{
  real<lower=0, upper=1> theta;
}
transformed parameters{
  vector[4] pi;
  pi[1] = 0.5 + 0.25*theta;
  pi[2] = 0.25*(1-theta);
  pi[3] = 0.25*(1-theta);
  pi[4] = 0.25*theta;
}
model{
  theta ~ normal(0.5, 0.2);
  y ~ multinomial(pi);
}
"
fit3 <- stan(
  model_code = scode3,
  data = list(
    y=c(125, 18, 20, 34)
  ),
  iter=2000
)

fit3

plot(fit3, plotfun="hist", pars="theta")

#T5

y <- c(
  0.77, NA, 0.53, NA, NA, 0.60, 0.64, 0.64, 0.67, 0.57, 0.68, 0.75,
  0.77, NA, NA, 0.65, 0.57, 0.54, 0.53, 0.84, 0.68, 0.82, NA, 0.51,
  0.77, 0.72, NA, 0.61, 0.56, 0.90, 0.52, NA, NA, NA, 0.50, 0.83
)

scode5 <- "
data{
  int<lower=0> n_obs;
  int<lower=0> n_mis;
  real y_obs[n_obs];
  real U;
}
parameters{
  real<lower=2> alpha;
}
model{
  alpha ~ normal(3,10);
  y_obs ~ pareto(0.5, alpha);
  target += n_mis*pareto_lccdf(U | 0.5, alpha);
}
"

fit5 <- stan(
  model_code = scode5,
  data=list(
    n_obs = sum(!is.na(y)),
    n_mis = sum(is.na(y)),
    y_obs=y[!is.na(y)],
    U=1.0
  ),
  iter=2000
)

fit5

plot(fit5, pars=c("alpha"), plotfun="hist")
