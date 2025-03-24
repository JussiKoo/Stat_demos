#T1

data <- read.csv2("empl.csv", header=T)

osuus <- data$unempl/data$population

#a

ue <- ts(osuus, start=1993, frequency=12)
plot(ue)
#b

ma_smooth <- function(x, window_size) {
  win <- rep(1/window_size, window_size) # painot
  stats::filter(x, win, sides=2)
}

ue_smooth <- ma_smooth(ue, 12)
ts.plot(ue, ue_smooth, gpars=list(xlab="vuosi", ylab="työttömien osuus", 
                                  col=c("blue","red")))

#c

ue_r <- ue - ue_smooth
ts.plot(ue_r, gpars=list(xlab="vuosi", ylab="jäännös"))

acf(ue_r, na.action=na.omit)

#Tätä voisi tulkita niin, että jäännökset eivät todennäköisesti 
#ole vain valkoista kohinaa. Peräkkäisten jäännösten käyttäytymisessä
#eri viiveillä on havaittavissa säännönmukaisuutta.

#===============================================================================

#T2

#a

m <- month.abb[cycle(ue)]

model <- lm(ue_r ~ m)
model

#b

plot(1:length(ue_r), ue_r, type="p", col="red")
lines(1:length(fitted(model)), fitted(model), type="l")

plot(resid(model))

#Residuaalit jakautuvat melko tasaisesti nollan ympärille. 
#Alhaisilla indekseillä on toisaalta havaittavissa hieman suurempia residuaaleja
#kuin muualla.

#c

ue.d <- decompose(ue)
plot(ue.d)

#Kausittainen vaihtelu näyttää hyvin samalta kuin sovitetun mallin antamat
#ennusteet

#d

ue.stl <- stl(ue, "per")
plot(ue.stl)

#Pohdinnat vielä
