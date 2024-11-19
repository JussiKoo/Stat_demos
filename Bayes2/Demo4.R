#T1

library(rstan)

url <- "http://users.jyu.fi/~santikka/bayes2/data/sideeffect.dat"
sideeffects <- read.table(url, header = TRUE)

scode1 <-
  "data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0> y[N];
  int<lower=0> n[N];
  real xtilde;
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
}"

#(logit(0.5)-a)/b = x


fit1 <- stan(
  model_code = scode1,
  data=list(
    N=nrow(sideeffects),
    x=sideeffects$annostus,
    n=sideeffects$potilaita,
    y=sideeffects$sivuvaik.,
    xtilde=1.5),
  iter=2000
)

summary(fit1)

#T2

y <- c(
  3.24, 1.42, 6.83, 3.60, 4.30, 0.81, 2.83, 1.01, 3.23, 5.99, 2.70, 2.09,
  4.88, 5.31, 0.44, 1.98, 0.19, 4.26, 2.39, 2.04, 1.29, 1.01, 2.57, 0.03
)

#T3

y <- c(
  3.24, 1.42, NA, 3.60, 4.30, 0.81, 2.83, 1.01, 3.23, 5.99, 2.70, 2.09,
  4.88, 5.31, 0.44, 1.98, 0.19, 4.26, 2.39, 2.04, 1.29, 1.01, 2.57, 0.03
)

#T4

y <- c(2, 18, 3, 21, 3, 2, 4, 3, 3, 11, 10, 30, 5, 4, 12, 49, 10, 1, 1, 26, 1, 6, 2, 6)

#Ensin posteriorinäytteisiin muunnos ja sitten lasketaan tehokkaat otoskoot.

#T5

y <- c(
  2.17, 4.04, 3.58, 2.42, 3.34, 1.89, 2.15, 2.39, 2.62, 1.96, 2.13, 2.61,
  1.90, 1.96, 1.81, 2.33, 2.25, 3.71, 1.78, 1.99, 2.71, 6.71, 2.36, 4.18
)

#T6

library("foreign")
url <- "http://users.jyu.fi/~santikka/bayes2/data/pisa2009FINs.sav"
pisadata <- read.spss(url, to.data.frame = TRUE