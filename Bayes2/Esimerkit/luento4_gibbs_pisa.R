# Esimerkki Gibbsin poimijasta hierarkkiselle mallille

# Malli:
# y_ij ~ N(theta_j, sigma2)
# theta_j ~ N(mu, omega2)
# p(mu) ~ 1
# omega2 ~ Inv-Gamma(A, B)
# sigma2 ~ Inv-Gamma(C, D)

set.seed(0)

library("haven")
library("coda")
library("dplyr")

# Aineisto
pisa <- read_sav("http://users.jyu.fi/~santikka/bayes2/data/pisa2009FINs.sav") |>
  arrange(SCHOOLID)
y <- pisa$readscore
n_total <- length(y)

# Koulukohtaisia tunnuslukuja
pisa_summary <- pisa |>
  group_by(SCHOOLID) |>
  summarise(mean = mean(readscore), n = n())
n <- pisa_summary$n # Oppilaiden lkm. eri kouluissa
y_mean <- pisa_summary$mean # Koulukohtaiset keskiarvot
m <- n_distinct(pisa$SCHOOLID) # Koulujen lkm

gibbs <- function(y, y_mean, n_total, n, m, n_iter, n_burnin,
                  mu_zero, omega2_zero, sigma2_zero, theta_zero) {
  # Hyperparametrit
  A <- 0.01
  B <- 0.01
  C <- 0.01
  D <- 0.01
  mu <- numeric(n_iter)
  omega2 <- numeric(n_iter)
  sigma2 <- numeric(n_iter)
  theta <- matrix(0, n_iter, m)
  mu[1] <- mu_zero
  omega2[1] <- omega2_zero
  sigma2[1] <- sigma2_zero
  theta[1, ]  <- theta_zero
  for (t in seq_len(n_iter - 1)) {
    # mu päivitys
    mu[t + 1] <- rnorm(1, mean(theta[t, ]), sqrt(omega2[t] / m))
    # omega^2 päivitys
    omega2[t + 1] <- 1 / rgamma(1, m / 2 + A, sum((theta[t, ] - mu[t + 1])^2) / 2 + B)
    # sigma^2 päivitys
    sigma2[t + 1] <- 1 / rgamma(1, n_total / 2 + C, sum((y - rep(theta[t, ], times = n))^2) / 2 + D)
    # theta päivitys
    vars <- 1 / (1 / omega2[t + 1] + n / sigma2[t + 1])
    means <- vars * ((1 / omega2[t + 1]) * mu[t + 1] + (n / sigma2[t + 1]) * y_mean)
    theta[t + 1, ] <- rnorm(m, means, sqrt(vars))
  }
  out <- cbind(mu, omega2, sigma2, theta)[(n_burnin + 1):n_iter, ]
  colnames(out) <- c("mu", "omega2", "sigma2", paste0("theta", 1:m))
  mcmc(out)
}

# Ajetaan 4 ketjua eri alkuarvoilla
chain1 <- gibbs(
  y, y_mean, n_total, n, m, n_iter = 10000, n_burnin = 2000,
  mu_zero = 500, omega2_zero = 1000, sigma2_zero = 100, theta_zero = rep(500, m)
)
chain2 <- gibbs(
  y, y_mean, n_total, n, m, n_iter = 10000, n_burnin = 2000,
  mu_zero = 600, omega2_zero = 900, sigma2_zero = 90, theta_zero = rep(400, m)
)
chain3 <- gibbs(
  y, y_mean, n_total, n, m, n_iter = 10000, n_burnin = 2000,
  mu_zero = 700, omega2_zero = 800, sigma2_zero = 80, theta_zero = rep(600, m)
)
chain4 <- gibbs(
  y, y_mean, n_total, n, m, n_iter = 10000, n_burnin = 2000,
  mu_zero = 800, omega2_zero = 700, sigma2_zero = 70, theta_zero = rep(300, m)
)

sims <- mcmc.list(list(chain1, chain2, chain3, chain4))

# Tulosten tarkastelua
plot.ts(sims[[1]][,1:10])
summ <- summary(sims)
summ$statistics[1:3,]
summ$quantiles[1:3,]
gelman.diag(sims) # Rhat diagnostiikka
eff <- effectiveSize(sims) # Tehokkaat otoskoot
min(eff)


