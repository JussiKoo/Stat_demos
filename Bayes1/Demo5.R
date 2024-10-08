library(brms)

#T3

#ennusteen odotusarvon posteriorikeskiarvo, ennusteen 95
#% ennustevC$li ja ennusteen odotusarvon 95 % posteriorivC$li

rain <- read.table("http://users.jyu.fi/~santikka/bayes1/data/rain.txt")

fitsade <- brm(joulukuu ~ marraskuu, data=rain)

newdata = data.frame(marraskuu=c(50))

#Ennusteen posteriorikeskiarvo ja sen 95% ennustevC$li
predict(fitsade, newdata = newdata)

#Ennusteen odotusarvon posteriorikeskiarvo 95% posteriorivC$li
fitted(fitsade, newdata = newdata)

#T4

#alpha vastaa tilannetta jossa demopisteet on 0. Tenttipisteet tuskin tulevat
#olemaan tällöin kovin suuret. Voisi olla esim N(0,1).

#beta kuvaa yhden demopistelisäyksen vaikutusta tenttipistemäärän odotusarvoon.
#Vaikutuksen voisi kuvitella olevan vähintään positiivinen. Voisi olla
#esim. N(4,1).

#Simuloidaan nC$ytteitC$ prioriennustejakaumasta

alphasim <- rnorm(1000, mean=0, sd=1)
betasim <- rnorm(1000, mean=4, sd=1)
musim <- alphasim + 3*betasim
sigmasim <- rgamma(1000,1,1)
ysim <- rnorm(1000, mean=musim, sd=sigmasim)
plot(density(ysim))
#T5

set.seed(1)

d <- data.frame(
  demopisteet = c(
    6, 5, 6, 5.5, 5.5, 6, 6, 6, 0, 6, 3,
    5, 4, 6, 4, 5, 5.5, 5.5, 2.5, 4, 0, 6, 0
  ),
  tentti = c(
    15, 20, 16, 15, 14, 21, 24, 17, 7, 17, 13,
    19, 14, 19, 11, 8, 11, 14, 12, 6, 5, 20, 4
  )
)

priors <- c(
  prior(normal(0,1), class="b", coef="Intercept"),
  prior(normal(4,1), class="b", coef="demopisteet"),
  prior(gamma(1,1), class="sigma")
)

fit <- brm(tentti ~ 0 + Intercept + demopisteet, prior = priors, data=d)
fit

#alpha posteriorikeskiarvo 0.85 ja 95% posterioriväli [-0.87, 2.53]
#beta posteriorikeskiarvo 2.84 ja 95% posterioriväli [2.39, 3.31]
#sigma posteriorikeskiarvo 3.82 ja 95% posterioriväli [2.89, 5.09]

#ehdolliset post. mediaanit
conditional_effects(fit)

#Havaintotason ennusteita
predict(fit)

#T6

fittrunc <- brm(tentti | trunc(lb=0, ub=24) ~ 0 + Intercept + demopisteet, prior = priors, data=d)
fittrunc

#ehdolliset posteriorimediaanit
conditional_effects(fittrunc)

#havaintotason ennusteita
predict(fittrunc)

