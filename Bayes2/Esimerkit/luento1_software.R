library("R2OpenBUGS")
library("rjags")
library("nimble")
library("rstan")
library("coda")
library("brms")

# Datan generointi
n <- 100
y <- rnorm(n, mean = 1, sd = 2)


# BUGS-toteutus #
bugsmodel <- function() {
  for (i in 1:n) {
    y[i] ~ dnorm(mu, tau)
  }
  mu ~ dnorm(1, 0.2)
  tau ~ dgamma(0.1, 0.01)
}

filename <- file.path(tempdir(), "bugsmodel.txt")
write.model(bugsmodel, filename)

inits <- function() { list(mu = 0, tau = 0.2) }

bugstul <- bugs(
  data = list("n", "y"),
  inits = inits,
  model.file = filename,
  parameters = c("mu", "tau"),
  n.chains = 1,
  n.iter = 1100,
  n.burnin = 100
)

bugstul
bugstul$sims.array[1:10, , ]

plot(bugstul$sims.array[, , "mu"], type = "l")
plot(bugstul$sims.array[, , "tau"], type = "l")

# JAGS-toteutus
model_text <- "
model {
  for (i in 1:n) {
    y[i] ~  dnorm(mu, tau)
  }
  mu ~ dnorm(1, 0.2)
  tau ~ dgamma(0.1, 0.01)
}
"

jagsmodel <- jags.model(
  file = textConnection(model_text),
  data = list(n = n, y = y),
  n.adapt = 1000
)
jagsout <- coda.samples(
  model = jagsmodel,
  variable.names = c("mu", "tau"),
  n.iter = 2000
)
summary(jagsout)
plot(jagsout)


# NIMBLE-toteutus
nimblecode <- nimbleCode({
  for (i in 1:n) {
    y[i] ~  dnorm(mu, tau)
  }
  mu ~ dnorm(1, 0.2)
  tau ~ dgamma(0.1, 0.01)
})

inits <- function() { list(mu = 0, tau = 0.2) }

nimbleout <- nimbleMCMC(
  code = nimblecode,
  constants = list(n = n),
  data = list(y = y),
  inits = inits,
  monitors = c("mu", "tau"),
  niter = 3000,
  nburnin = 1000,
  summary = TRUE,
  samplesAsCodaMCMC = TRUE
)

nimbleout$summary
plot(nimbleout$samples)


# Stan-toteutus
stanmodel <- "
  data {
    int<lower=0> n;
    real y[n];
  }
  parameters {
    real mu;
    real tau;
  }
  model {
    mu ~ normal(1, 1 / sqrt(0.2));
    tau ~ normal(0, 10);
    y ~ normal(mu, 1 / sqrt(tau));
  }
"

stanout <- stan(
  model_code = stanmodel,
  model_name = "esimerkki",
  data = list(n = n, y = y),
  chains = 1,
  iter = 2000,
  warmup = 1000
)

plot(stanout, plotfun = "hist", bins = 20)

# Esimerkki brms-paketin generoimasta Stan-koodista
pisa <- read.table("http://users.jyu.fi/~santikka/bayes1/data/pisa.txt", header = TRUE)

fit_pisa <- brm(
  mpist ~ matem + sukup + koulusij + koulualue + aidink + SES,
  data = pisa,
  refresh = 0,
  chains = 1
)

fit_pisa2 <- brm(
  mpist ~ 0 + Intercept + matem + sukup + koulusij + koulualue + aidink + SES,
  data = pisa,
  refresh = 0,
  chains = 1
)

# PISA-mallin priorit (brms:n oletuspriorit) eri parametrisaatioilla
prior_summary(fit_pisa)
prior_summary(fit_pisa2)

# PISA-mallin Stan-koodi eri parametrisaatioilla
stancode(fit_pisa)
stancode(fit_pisa2)
