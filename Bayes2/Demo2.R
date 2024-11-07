#T1

#a) target-"muuttuja" kuvaa log-(posteriori)tiheysfunktiota

#===============================================================================

#b) "~"-notaation jälkeinen lauseke lisätään posterioritiheyteen kertoimena.
#Eli tämä vastaa sikäli lisäämistä lisäystä "target"-muuttujaan.

#Seuraavat rivit vastaavat johtavat siis samaan päättelyyn

#y ~ normal(mu, sigma);

#target += normal_lpdf(y | mu, sigma);

#===============================================================================

#c)

#Normalisoimattomien jakaumien käyttö tekee simuloinnista laskennallisesti
#edullisempaa.

#T3

library(rstan)
library(tidyr)

y <- c(1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0)

#(theta2)^6 * (theta1)^3 * (1-theta2)^2 * (1-theta1)^8

scode3 <- 
"data {
  int<lower=0> N;
  int y[N];
}
parameters {
  real theta1;
  real theta2;
}
model {
  theta1 ~ uniform(0,1);
  theta2 ~ uniform(0,1);
  for (n in 2:N){
    if (y[n-1] == 0) y[n] ~ bernoulli(theta1);
    else y[n] ~ bernoulli(theta2);
  }
}"

fit3 <- stan(
  model_code = scode3,
  data = list(N = length(y), y = y),
  iter = 2000
)

params <- extract(fit3)

theta1est <- mean(params$theta1)
theta2est <- mean(params$theta2)

Ptheta <- matrix(
  c(
    1-theta1est, 1-theta2est,
    theta1est, theta2est
  ),
  2,
  2
)

Ptheta

#T4

url <- "http://users.jyu.fi/~santikka/bayes2/data/lankarulla.dat"
lankarulla <- read.table(url, header = TRUE)
lankarulla$pituus <- lankarulla$pituus/100
eka <- lankarulla[1,2]
lankarulla[1,2] <- NA
lankarulla <- drop_na(lankarulla)

lankarulla


#Miten toimitaan? Käytetäänkö sovittamiseen kaikkia muita rivejä vai
#käsitelläänkö tuo yksi havainto ikään kuin sensuroituna?

scode4 <- 
  "data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0> y[N];
  real x_tilde;
}
parameters {
  real alpha;
  real beta;
}
model {
  alpha ~ normal(0,100);
  beta ~ normal(0,100);
  y ~ poisson_log(alpha+beta*x);
}
generated quantities {
  int<lower=0> y_tilde = poisson_log_rng(alpha + beta * x_tilde);
}"

fit4 <- stan(
  model_code = scode4,
  data = list(N = nrow(lankarulla), y = lankarulla$virheet, x = lankarulla$pituus, x_tilde = 5.51),
  iter=2000
  )

fit4

params4 <- rstan::extract(fit4)

params4$y_tilde

#Posterioriennustejakauma kuvaa tässä tapauksessa "uuteen" havaintoon liittyvää
#epävarmuutta. Epävarmuus liittyy tässä tapauksessa parametreihin, että havaintoihin.


#T5

#T6