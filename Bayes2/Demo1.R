#Bayes 2 - demo 1

#T4

#Annetaan input otos jakaumasta

#Järjestetään otos

#Lasketaan min ja max

#Kuljetetaan alarajaa minimistä lähtien ja lasketaan mihin asti tulisi katsoa
#jotta mukaan on laskettu 95% otoksesta. Talletetaan välin pituus.

#Päivitetään välin pituus jos löytyy pienempi.

lyhinvali <- function(otos) {
  jarjestetty_otos <- sort(otos)
  
  n <- length(otos)
  valin_pituus <- ceiling(0.95 * n)
  
  parasalaraja <- 0
  parasylaraja <- 0
  lyhinpituus <- Inf
  
  for (i in 1:(n-valin_pituus)) {
    pituus <- jarjestetty_otos[i + valin_pituus] - jarjestetty_otos[i]
    if (pituus < lyhinpituus) {
      parasalaraja <- jarjestetty_otos[i]
      parasylaraja <- jarjestetty_otos[i + valin_pituus]
      lyhinpituus <- pituus
    }
  }
  
  return(c(parasalaraja, parasylaraja))
}

#Testataan

rg <- rgamma(1000, shape=1, rate=0.2)

hdi(rg)
lyhinvali(rg)

rn <- rnorm(1000, mean = 10, sd=2)

hdi(rn)
lyhinvali(rn)

#Pohdintaa

#Tämänlainen posterioriväli on varmaan ainakin sellaisessa tapauksessa hyvä kun
#jakaumalla on yksi "huippu".


#T5

#a

library(ggplot2)
library(dplyr)

mobilephonedata <- read.csv("mobile_phone_purchases.csv", sep=';') |>
  select(
    Ika,
    Sukup,
    nyk_hankintavuosi = Q43,
    nyk_hankintakuukausi = Q44,
    nyk_hankintavuodenaika  = Q45,
    ent_hankintavuosi = Q48,
    ent_hankintakuukausi = Q49,
    ent_hankintavuodenaika  = Q50,
  )

mobilephonedata$nyk_min_hankintavuosi <- rep(NA, nrow(mobilephonedata))

for (i in 1:nrow(mobilephonedata)) {
  if (mobilephonedata$nyk_hankintavuosi[i] != 9999) 
    mobilephonedata$nyk_min_hankintavuosi[i] <- mobilephonedata$nyk_hankintavuosi[i]
  else if (mobilephonedata$ent_hankintavuosi[i] != 9999 && !is.na(mobilephonedata$ent_hankintavuosi[i]))
    mobilephonedata$nyk_min_hankintavuosi[i] <- mobilephonedata$ent_hankintavuosi[i]
  else mobilephonedata$nyk_min_hankintavuosi[i] <- 2000 #tätä voisi vielä säätää
}

mobilephonedata$nyk_max_hankintavuosi <- rep(NA, nrow(mobilephonedata))

for (i in 1:nrow(mobilephonedata)) {
  if (mobilephonedata$nyk_hankintavuosi[i] != 9999) 
    mobilephonedata$nyk_max_hankintavuosi[i] <- mobilephonedata$nyk_hankintavuosi[i]
  else mobilephonedata$nyk_max_hankintavuosi[i] <- 2012
}

mobilephonedata$nyk_min_hankintakuukausi <- rep(NA, nrow(mobilephonedata))

for (i in 1:nrow(mobilephonedata)) {
  if (mobilephonedata$nyk_hankintakuukausi[i] != 13) 
    mobilephonedata$nyk_min_hankintakuukausi[i] <- mobilephonedata$nyk_hankintakuukausi[i]-1
  else if (mobilephonedata$nyk_hankintavuodenaika[i] != 5)
    mobilephonedata$nyk_min_hankintakuukausi[i] <- (mobilephonedata$nyk_hankintavuodenaika[i]-1)*3
  else
    mobilephonedata$nyk_min_hankintakuukausi[i] <- 0
}

mobilephonedata$nyk_max_hankintakuukausi <- rep(NA, nrow(mobilephonedata))

for (i in 1:nrow(mobilephonedata)) {
  if (mobilephonedata$nyk_hankintakuukausi[i] != 13) 
    mobilephonedata$nyk_max_hankintakuukausi[i] <- mobilephonedata$nyk_hankintakuukausi[i]
  else if (mobilephonedata$nyk_hankintavuodenaika[i] != 5) {
    if (mobilephonedata$nyk_max_hankintavuosi[i] == 2013)
      mobilephonedata$nyk_max_hankintakuukausi[i] <- 2
    else mobilephonedata$nyk_max_hankintakuukausi[i] <- mobilephonedata$nyk_hankintavuodenaika[i]*3
  }
  else
    mobilephonedata$nyk_max_hankintakuukausi[i] <- 12
}

#Lasketaan mikä on min ja max käyttöaika

mobilephonedata$nyk_min_kayttoaika <- (2012-mobilephonedata$nyk_max_hankintavuosi)*12 + (12-mobilephonedata$nyk_max_hankintakuukausi) + 2
mobilephonedata$nyk_max_kayttoaika <- (2012-mobilephonedata$nyk_min_hankintavuosi)*12 + (12-mobilephonedata$nyk_min_hankintakuukausi) + 2

#b

mean(mobilephonedata$nyk_min_kayttoaika)
mean(mobilephonedata$nyk_max_kayttoaika)

ggplot(data=mobilephonedata, mapping=aes(x=nyk_min_kayttoaika, label="Min käyttöaika"))+
  geom_density(color="blue")+
  geom_density(mapping=aes(x=nyk_max_kayttoaika), color="red")+
  xlim(c(0,200))+
  xlab("Käyttöaika")

#T6

library(rstan)
scode <- "
parameters {
  array[2] real y;
}
model {
  y[1] ~ normal(0, 1);
  y[2] ~ double_exponential(0, 2);
}
"
fit1 <- stan(model_code = scode, iter = 10, verbose = FALSE)
print(fit1)










