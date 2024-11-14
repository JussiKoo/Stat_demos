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

params <- rstan::extract(fit3)

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
lankarulla$pituuska <- lankarulla$pituus - mean(lankarulla$pituus)
eka <- lankarulla[1,2]
lankarulla[1,2] <- NA
lankarulla <- drop_na(lankarulla)

mean(lankarulla$pituus)

plot(virheet ~ pituuska, data=lankarulla)


#Miten toimitaan? Käytetäänkö sovittamiseen kaikkia muita rivejä vai
#käsitelläänkö tuo yksi havainto ikään kuin sensuroituna?

#Priorit:
#exp(alpha) kuvaa kuinka paljon 5.88 m mittaisessa rullassa on keskimäärin virheitä
#Asetetaan prioriksi normal(log(7), 10)

#exp(beta) kuvaa prosentuaalista muutosta virheiden lukumäärässä kun lankarullan pituus kasvaa 1 m.
#Asetetaan prioriksi normal(log(1.2, 10))

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
  alpha ~ normal(2, 10);
  beta ~ normal(0.2, 10);
  y ~ poisson_log(alpha+beta*x);
}
generated quantities {
  int<lower=0> y_tilde = poisson_log_rng(alpha + beta * x_tilde);
}"

fit4 <- stan(
  model_code = scode4,
  data = list(N = nrow(lankarulla), y = lankarulla$virheet, x = lankarulla$pituuska, x_tilde = 5.51-mean(lankarulla$pituus)),
  iter=2000
  )

fit4

params4 <- rstan::extract(fit4)

params4$y_tilde

#Posterioriennustejakauma kuvaa tässä tapauksessa "uuteen" havaintoon liittyvää
#epävarmuutta. Epävarmuus liittyy tässä tapauksessa parametreihin, että havaintoihin.


#T5

url <- "http://users.jyu.fi/~santikka/bayes2/data/erityispedagogiikka.dat"
ep <- read.table(url, header = TRUE)

head(ep)

scode5 <-
"data {
  int<lower=0> N;
  vector[N] x;
  array[N] int<lower=0> n;
  array[N] int<lower=0> y;
  real<lower=0> x_tilde;
}
parameters {
  real alpha;
  real beta;
}
model {
  alpha ~ normal(1.5, 10);
  beta ~ normal(-0.5, 10);
  y ~ binomial_logit(n, alpha + beta * x);
}
generated quantities {
  real<lower=0, upper=1> pred = binomial_rng(1, inv_logit(alpha + beta * x_tilde));
}"



fit5 <- stan(
  model_code = scode5,
  data=list(N=nrow(ep), x=ep$ika, n=ep$yht, y=ep$onn, x_tilde=9),
  iter=2000
)

fit5

#b)

params5 <- rstan::extract(fit5)

mean(exp(params5$beta))

#Ikien i+1 ja i välisen odds ration posteriorikeskiarvo on 0.767 joten näyttäisi,
#että vanhemmilla sijoitettavilla on onnistumisen todennäköisyys on huonompi.

#c)

mean(params5$pred)

#T6

scode6 <-
  "data {
  int<lower=0> N;
  vector[N] x;
  array[N] int<lower=0> n;
  array[N] int<lower=0> y;
  real<lower=0> x_tilde;
}
parameters {
  real alpha;
  real beta;
}
model {
  alpha ~ normal(1.5, 10);
  beta ~ normal(-0.5, 10);
  y ~ binomial(n, inv_cloglog(alpha + beta * x));
}
generated quantities {
  real<lower=0, upper=1> pred = binomial_rng(1, inv_cloglog(alpha + beta * x_tilde));
}"

#1-exp(-exp(alpha + beta * x))

fit6 <- stan(
  model_code = scode6,
  data=list(N=nrow(ep), x=ep$ika, n=ep$yht, y=ep$onn, x_tilde=9),
  iter=2000
)

fit5

fit6

params6 <- rstan::extract(fit6)

mean(exp(params6$beta))

mean(params6$pred)

#Molemmilla linkkifunktioilla posterioriennustejakauman avulla laskettu
#estimaatti todennäköisyydelle on melkein sama.
