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

