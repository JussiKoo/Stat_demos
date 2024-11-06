# Bayes 2, syksy 2023
# PISA-aineisto: hierkkinen malli
# vrt. luentomoniste, esim. 2.1

library("foreign")
library("rstan")
library("dplyr")

url <- "http://users.jyu.fi/~santikka/bayes2/data/pisa2009FINs.sav"
pisadata <- read.spss(url, to.data.frame = TRUE)

attributes(pisadata)$variable.labels[1:10]

# SCHOOLID
# "koulun numero"
# STUDID
# "oppilaan numero"
# ST04Q01
# "sukupuoli"
# readscore
# "lukutaidon pistemäärä"
# mathscore
# "matematiikan pistemäärä"
# sciescore
# "luonnontieteiden pistemäärä"
# ST24Q07
# "käyn mielelläni kirjakaupassa tai kirjastossa"

summary(pisadata$readscore)
summary(pisadata$mathscore)
summary(pisadata$sciescore)
summary(pisadata$ST04Q01)
summary(pisadata$SCHOOLID)

hist(pisadata$readscore)
hist(pisadata$mathscore)
hist(pisadata$sciescore)
pairs(pisadata[,c("readscore", "mathscore", "sciescore")])
# summary(pisadata$ST24Q07 )

d <- pisadata |>
  select(STUDID, SCHOOLID, ST04Q01, readscore, mathscore, sciescore)


# Stan-malli 1: kokeilua

scode1 <- "
data {
  int<lower=0> n;
  real readscore[n];
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  readscore ~ normal(mu, sigma);
}
"

fit1 <- stan(
  model_code = scode1,
  data = list(n = nrow(d), readscore = d$readscore),
  iter = 2000
)

print(fit1, probs = c(0.025, 0.5, 0.975))
plot(fit1)
plot(fit1, show_density = TRUE, ci_level = 0.8, fill_color = "purple")
plot(fit1, plotfun = "hist")
pairs(fit1, pars = c("mu", "sigma"))
plot(fit1, plotfun = "trace", inc_warmup = TRUE)


# Stan-malli 2: hierarkkinen malli, jossa mukana koulu

scode2 <- "
data {
  int<lower=0> n;         // oppilaiden määrä
  int<lower=0> m;         // koulujen määrä
  int<lower=0> school[n]; // oppilaan koulu
  real readscore[n];      // lukutaitopisteet
}
parameters {
  real<lower=0, upper=1000> mu;
  real theta[m];
  real<lower=0> sigma2;
  real<lower=0> omega2;
}
model {
  sigma2 ~ inv_gamma(0.01, 0.01);
  omega2 ~ inv_gamma(0.01, 0.01);
  theta ~ normal(mu, sqrt(omega2));
  for (i in 1:n) {
    readscore[i] ~ normal(theta[school[i]], sqrt(sigma2));
  }
}
"

inits <- function() {
  list(mu = 500, theta = rep(500, length(unique(d$SCHOOLID))), sigma2 = 10000, omega2 = 1000)
}

fit2 <- stan(
  model_code = scode2,
  data = list(
    n = nrow(d),
    m = length(unique(d$SCHOOLID)), # Koulujen lkm,
    readscore = d$readscore,
    school = as.integer(factor(d$SCHOOLID))
  ),
  init = inits,
  iter = 2000
)

print(fit2)
print(fit2, pars = c("mu", "sigma2", "omega2"))
print(fit2, pars = "theta")
plot(fit2)
plot(fit2, pars = c("mu", "sigma2", "omega2"))
plot(fit2, show_density = TRUE, ci_level = 0.8, fill_color = "purple")
plot(fit2, plotfun = "hist", pars = c("mu", "sigma2", "omega2"))
pairs(fit2, pars = c("mu", "sigma2", "omega2"))
plot(fit2, plotfun = "trace", pars = c("mu", "sigma2", "omega2"), inc_warmup = TRUE)
plot(fit2, plotfun = "trace", pars = c("mu", "sigma2", "omega2"), inc_warmup = TRUE, window = c(1, 100))
plot(fit2, plotfun = "rhat")



# Stan-malli 3: hierarkkinen malli, jossa mukana koulu ja sukupuoli

scode3 <- "
data {
  int<lower=0> n;                 // oppilaiden määrä
  int<lower=0> m;                 // koulujen määrä
  int<lower=0, upper=1> sukup[n]; // oppilaan sukupuoli
  int<lower=0> koulu[n];          // oppilaan koulu
  real readscore[n];              // lukutaitopisteet
}
parameters {
  real<lower=0, upper=1000> mu;
  real beta_koulu[m];
  real beta_sukup;
  real<lower=0> sigma2;
  real<lower=0> omega2;
}
model {
  sigma2 ~ inv_gamma(0.01, 0.01);
  omega2 ~ inv_gamma(0.01, 0.01);
  beta_koulu ~ normal(mu, sqrt(omega2));
  for (i in 1:n) {
    readscore[i] ~ normal(beta_koulu[koulu[i]] + beta_sukup * sukup[i], sqrt(sigma2));
  }
}
"

inits <- function() {
  list(mu = 500, beta_koulu = rep(500, length(unique(d$SCHOOLID))), beta_sukup = 0, sigma2 = 5000, omega2 = 500)
}

fit3 <- stan(
  model_code = scode3,
  data = list(
    n = nrow(d),
    m = length(unique(d$SCHOOLID)), # Koulujen lkm,
    readscore = d$readscore,
    koulu = as.integer(factor(d$SCHOOLID)),
    sukup = 2L - as.integer(factor(d$ST04Q01))
  ),
  init = inits,
  iter = 2000
)

print(fit3, pars = c("mu", "beta_sukup", "sigma2", "omega2"))
plot(fit3, pars = c("mu", "beta_sukup", "sigma2", "omega2"))
plot(fit3, pars = c("mu", "beta_sukup", "sigma2", "omega2"), show_density = TRUE, ci_level = 0.8, fill_color = "purple")
plot(fit3, plotfun = "hist", pars = c("mu", "beta_sukup", "sigma2", "omega2"))
pairs(fit3, pars = c("mu", "beta_sukup", "sigma2", "omega2"))
plot(fit3, plotfun = "trace", pars = c("mu", "beta_sukup", "sigma2", "omega2"), inc_warmup = TRUE)
plot(fit3, plotfun = "rhat")
