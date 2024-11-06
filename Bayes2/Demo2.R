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

y <- c(1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0)
length(y)

n1 <- 6 #1,1 parit
n2 <- 8 #0,0 parit
n3 <- 3 #1,0 parit
n4 <- 2 #0,1 parit

#Uskottavuus: 

#(theta2)^6 * (theta1)^3 * (1-theta2)^2 * (1-theta1)^8

scode <- 
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

fit1 <- stan(
  model_code = scode,
  data = list(N = length(y), y = y),
  iter = 2000
)

params <- extract(fit1)

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

#T5

#T6