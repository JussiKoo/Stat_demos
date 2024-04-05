#T1

library(metafor)

data <- dat.normand1999

#Results from 9 studies on the length of the hospital stay of stroke patients
#under specialized care and under conventional/routine (non-specialist) care.

attach(data)

dat <- escalc(measure = "MD", 
              n1i=n1i, 
              m1i=m1i, 
              sd1i=sd1i,
              sd2i=sd2i,
              n2i=n2i,
              m2i=m2i,
              data = dat.normand1999, append = TRUE)

print(dat, row.names = FALSE)

#Kiinteiden vaikutusten malli

fem <- rma(dat$yi, dat$vi, data = dat, method = "FE")
print(fem)

#Tutkimuksissa erikoishoitoa saaneet potilaat viettivät sairaalassa keskimäärin
# noin 3,46 päivää vähemmän kuin tavallista hoitoa saaneet

forest(fem)

# Satunnaisten vaikutusten malli
rem <- rma(dat$yi, dat$vi, data = dat)

print(rem)
print(confint(rem))

forest(rem)

#T3

dat2 <- escalc(measure="SMD",
              m1i=c(16,44,0.134,2.8),
              sd1i=c(0.1,0.1,0.1,2.4),
              m2i=c(1,224,0.138,3.2),
              sd2i=c(0.1,0.1,0.1,2.1),
              n1i=c(16,44,72,28),
              n2i=c(1,224,1,32),
              sei=c(142.37,-1794.92,-0.04,-0.18))

fem2 <- rma(dat2$yi, dat2$vi, weights = c(0.119, 0.02, 0.43, 0.431), data = dat2, method = "FE")
summary(fem2)
forest(fem2)
