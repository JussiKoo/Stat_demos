#T1

#P(vähintään yhdestä testistä p<0.05) = 1 - P(kaikista testeistä p>=0.05)

#Ei monitestauskorjausta

m <- 1:100

tn1 <- 1 - 0.95^m

#Bonferroni-korjaus

#Kaikki nimelliset p-arvot kerrotaan otoskoolla.
#Joten esimerkiksi kun m=100 saadaan 
#P(testistä saadaan p>=0.05) = 1 - 0.05/100

tn2 <- 1 - (1-0.05/m)^m

plot(1:100, tn1)
points(1:100, tn2, col="red")

#===============================================================================

#T2

#===============================================================================

#Testisuure Z_k
#Nollahypoteesin vallitessa noudattaa N(0,1)
#Vaihtoehtoisen vallitessa noudattaa N(3,1)

#testattavien hypoteesien määrä m=100000
#0.1% tilanteissa vaihtoehtoinen hypoteesi on tosi.

set.seed(2019)

m <- 100000
alpha <- 0.025

z1 <- rnorm(99900,0,1)
z2 <- rnorm(100,3,1)

z <- c(z1, z2)

z <- sample(z)
reject <- numeric(m)

p.values <- numeric(m)

#a

for (i in 1:m){
  p.values[i] <- 1-pnorm(abs(z[i]), mean = 0, sd = 1)
  
  reject[i] <- p.values[i]*m < alpha
}

sum(reject)

#b

p.values.sorted <- sort(p.values)

i <- 1

while (p.values.sorted[i] <= (i/m)*0.2){
  i <- i+1
}

i

#================

#T3

priori <- 0.01

ptautipos <- (0.85*priori)/(0.85*priori + 0.04*(1-priori))

ptautineg <- (0.15*priori)/(0.15*priori + 0.96*(1-priori))

100*ptautipos+900*ptautineg

#=========================

#T4

set.seed(20190109)
x <- rexp(45, rate = 1.6)

thetahat <- length(x)/sum(x)
thetanull <- 1.2

L <- function(theta, data){
  n <- length(data)
  
  return(theta^n*exp(-theta*sum(data)))
}

dl <- function(theta, data){
  n <- length(data)
  
  return(n/theta - sum(data))
}

#a Uskottavuusosamäärän testi

LR <- L(thetanull, x)/L(thetahat, x)

D <- -2*log(LR)

p1 <- 1-pchisq(D, df=1)

#b Waldin testi

ZW <- (thetahat-thetanull)/(thetahat/sqrt(length(x)))

p2 <- 2*(1-pnorm(ZW,0,1))

#c Raon skooritesti

ddl <- function(theta, data){
  n <- length(data)
  
  return(-n/theta^2)
}

ZR <- dl(thetanull, x)/sqrt(-ddl(thetanull, x))

p3 <- 2*(1 - pnorm(ZR,0,1))

cat("Uskottavuusosamäärän testi: ", p1, "\nWaldin testi: ", p2, "\nRaon skooritesti: ", p3)

#T5

simn <- 10000
simD <- rep(NA,simn)
simZW <- rep(NA,simn)
simZR <- rep(NA,simn)
for(i in 1:simn)
{
  xsim <- rexp(45, rate=1.2)
  thetahatsim <- length(x)/sum(x)
  
  simD[i] <- -2*log(L(thetanull, xsim)/L(thetahatsim, xsim))
  simZW[i] <- (thetahatsim-thetanull)/(thetahatsim/sqrt(length(x)))
  simZR[i] <- dl(thetanull, xsim)/sqrt(-ddl(thetanull, xsim))
}  

sum(D < simD)/simn
2*sum(ZW < simZW)/simn 
2*sum(ZR < simZR)/simn

cat("Uskottavuusosamäärän testi: ", p1, "\nWaldin testi: ", p2, "\nRaon skooritesti: ", p3)

#T6

simn <- 100000

simthetahat <- rep(NA,simn)

for(i in 1:simn){
  simthetahat[i] <- 45/rgamma(1, shape=45, rate=1.2)
}

#Lasketaan moniko simuloitu estimaatti on suurempi kuin alkuperäinen.
sum(simthetahat > thetahat)/simn

#=====================================

library(metafor)

dat2 <- escalc(measure="SMD",
               m1i=c(16,44,0.134,2.8),
               sd1i=c(0.1,0.1,0.1,2.4),
               m2i=c(1,224,0.138,3.2),
               sd2i=c(0.1,0.1,0.1,2.1),
               n1i=c(16,44,72,28),
               n2i=c(1,224,1,32))

fem2 <- rma(dat2$yi, dat2$vi, weights = c(0.119, 0.02, 0.43, 0.431), data = dat2, method = "FE")
summary(fem2)
forest(fem2)


