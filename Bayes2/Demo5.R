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

url <- "http://users.jyu.fi/~santikka/bayes2/data/Margus_etal_data_gen_2.csv"
beetle <- read.csv2(url)
head(beetle)

#Vasteet Weight_7d_mg ja Ad_weight_mg

#Mallinnus moniulotteisella normaalijakaumalla

#odotusarvovektoriin selittäjinä Sex ja Treatment

scode <- "
data {
  int<lower=0> n;         // kuoriaisten määrä
  int<lower=0, upper=1> sex[n]; // kuoriaisen sukupuoli
  int<lower=0, upper=1> treatment[n]; //käsittely
  real 7d_weight[n];
  real adultweight[n];
}
transformed data {
  vector[2] y[n];
  for (i in 1:n) {
    y[i, 1] = 7d_weight[i];
    y[i, 2] = adultweight[i];
  }
}
parameters {
  // hierarkkiset parametrit
  vector[2] mu;
  cholesky_factor_corr[2] K;

  // koulukohtaiset parametrit
  vector[2] beta_1;
  vector[2] beta_2;

  // kaikille oppilaille yhteiset parametrit
  cholesky_factor_corr[2] L;
  vector<lower=0>[3] sigma;
}
model {
  vector[3] theta_s[n];
  for (i in 1:n) {
    theta_s[i] = theta[school[i]];
  }

  mu ~ normal(500, 100);
  K ~ lkj_corr_cholesky(2.0);
  omega ~ inv_gamma(1, 5);

  theta ~ multi_normal_cholesky(mu, diag_pre_multiply(omega, K));

  L ~ lkj_corr_cholesky(2.0);
  sigma ~ inv_gamma(1, 5);

  y ~ multi_normal_cholesky(theta_s, diag_pre_multiply(sigma, L));
}
"