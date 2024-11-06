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

y <- c(1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0)
length(y)

n1 <- 6 #1,1 parit
n2 <- 8 #0,0 parit
n3 <- 3 #1,0 parit
n4 <- 2 #0,1 parit

#Uskottavuus: 

#(theta2)^6 * (theta1)^3 * (1-theta2)^2 * (1-theta1)^8

stan <- 
"data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real theta1;
  real theta2;
}
model {
  theta1 ~ uniform(0,1);
  theta2 ~ uniform(0,1);
  y ~ normal(alpha + beta * x, sigma);
}"




#T4

#T5

#T6