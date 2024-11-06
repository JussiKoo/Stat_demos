# Simulointi normaalijakaumasta Gibbsin otannalla

library("ggplot2")

norm_sim <- function(mu, S, x_0, n_iter, n_burnin) {
  x <- matrix(ncol = 2, nrow = n_iter)
  x[1, ] <- x_0
  s1 <- sqrt(S[1, 1])    # x1:n keskihajonta
  s2 <- sqrt(S[2, 2])    # x2:n keskihajonta
  s12 <- S[1, 2]         # kovarianssi
  rho <- s12 / (s1 * s2) # korrelaatiokerroin
  for (t in seq_len(n_iter - 1)) {
    x[t + 1, 1] <- rnorm(
      n = 1,
      mean = mu[1] + s1 / s2 * rho * (x[t, 2] - mu[2]),
      sd = sqrt(1 - rho^2) * s1
    )
    x[t + 1, 2] <- rnorm(
      n = 1,
      mean = mu[2] + s2 / s1 * rho * (x[t + 1, 1] - mu[1]),
      sd = sqrt(1 - rho^2) * s2
    )
  }
  x[(n_burnin + 1):n_iter, ]
}

# Parametrien asettaminen
mu <- c(4, 1)
S <- matrix(c(25, 10.5, 10.5, 9), ncol = 2)
# S <- matrix(c(25, 14.9, 14.9, 9), ncol = 2)

# Simulointi
tul <- norm_sim(mu, S, c(-100, 100), 1000, 100)

df_norm_gibbs <- as.data.frame(tul)
df_norm_gibbs[ ,3] <- 1:nrow(df_norm_gibbs)
colnames(df_norm_gibbs) <- c("x_1", "x_2", "ix")

# Reunajakaumat
ggplot(df_norm_gibbs, aes(x = x_1, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "lightgray", binwidth = 2) +
  labs(
    title = expression(x[1] * " Reunaposteriori"),
    x = expression(x[1]),
    y = "Tiheys") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = c(0, 0))

ggplot(df_norm_gibbs, aes(x = x_2, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "lightgray", binwidth = 1.5) +
  labs(
    title = expression(x[2] * " Reunaposteriori"),
    x = expression(x[2]),
    y = "Tiheys") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = c(0, 0))

# Jälkikuviot
ggplot(df_norm_gibbs, aes(x = ix, y = x_1)) +
  geom_line() +
  labs(title = expression(x[1]), x = "Index", y = expression(x[1])) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df_norm_gibbs, aes(x = ix, y = x_2)) +
  geom_line() +
  labs(title = expression(x[2]), x = "Index", y = expression(x[2])) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

# Hajontakuvio
ggplot(df_norm_gibbs, aes(x = x_1, y = x_2)) +
  geom_point(size = 0.75) +
  labs(
    title = "Hajontakuvio",
    x = expression(x[1]),
    y = expression(x[2])) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


# Simulointia mvtnorm paketin avulla

x <- mvtnorm::rmvnorm(1000, mean = mu, sigma = S)
plot(x)
