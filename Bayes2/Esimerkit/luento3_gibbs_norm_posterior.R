# Posteriorijakauman p(\mu, \sigma^2|y) simulointi Gibbsin otannalla

gibbs_norm <- function(y, mu_0, sigma2_0, n_sim, n_burn) {
  mu <- vector(length = n_sim)
  mu[1] <- mu_0
  sigma2 <- vector(length = n_sim)
  sigma2[1] <- sigma2_0
  y_mean <- mean(y)
  n <- length(y)
  for (t in seq_len(n_sim - 1)) {
    mu[t + 1] <- rnorm(1, mean = y_mean, sd = sqrt(sigma2[t] / n))
    sigma2[t + 1] <- 1 / rgamma(1, n / 2, 0.5 * sum((y - mu[t + 1])^2))
  }
  list(
    mu = mu[seq(n_burn + 1, n_sim)],
    sigma2 = sigma2[seq(n_burn + 1, n_sim)]
  )
}

# Esimerkkiaineisto
mu <- 3
sigma <- 5
y <- rnorm(100, mu, sigma)

# Simulointi
sim <- gibbs_norm(y = y, mu_0 = -100, sigma2_0 = 1000, n_sim = 1000, n_burn = 0)

plot(sim$mu, type = "l")
plot(sim$sigma2, type = "l")

hist(sim$mu)
hist(sim$sigma2)

quantile(sim$mu, probs = c(0.025, 0.975))
quantile(sim$sigma2, probs = c(0.025, 0.975))

