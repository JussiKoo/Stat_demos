library(brms)

#T3

#ennusteen odotusarvon posteriorikeskiarvo, ennusteen 95
#% ennusteväli ja ennusteen odotusarvon 95 % posterioriväli

rain <- read.table("http://users.jyu.fi/~santikka/bayes1/data/rain.txt")

fitsade <- brm(joulukuu ~ marraskuu, data=rain)

newdata = data.frame(marraskuu=c(50))

#Ennusteen posteriorikeskiarvo ja sen 95% ennusteväli
predict(fitsade, newdata = newdata)

#Ennusteen odotusarvon posteriorikeskiarvo 95% posterioriväli
fitted(fitsade, newdata = newdata)

#T4

#alpha vastaa tilannetta jossa demopisteet on 0. Tenttipisteet tuskin tulevat
#olemaan tällöin kovin suuret. Voisi olla esim N(1,1).

#beta kuvaa yhden demopistelisäyksen vaikutusta tenttipistemäärän odotusarvoon.
#Vaikutuksen voisi kuvitella olevan vähintään positiivinen. Voisi olla
#esim. N(4,1).

#Simuloidaan näytteitä prioriennustejakaumasta

alphasim <- rnorm(1000, mean=1, sd=1)
betasim <- rnorm(1000, mean=4, sd=1)
musim <- alphasim + 3*betasim
ysim <- rnorm(1000, mean=musim, sd=1)
plot(density(ysim))

#T5

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

#Luento 7.10

#Mallinnusjuttuja


