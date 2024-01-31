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

#T2

#Testisuure Z_k
#Nollahypoteesin vallitessa noudattaa N(0,1)
#Vaihtoehtoisen vallitessa noudattaa N(3,1)

#testattavien hypoteesien määrä m=100000
#0.1% tilanteissa vaihtoehtoinen hypoteesi on tosi.

set.seed(2019)

m <- 100000
alpha <- 0.05

z1 <- rnorm(99900,0,1)
z2 <- rnorm(100,3,1)

z <- c(z1, z2)

z <- sample(z)
reject <- numeric(m)

p.values <- numeric(m)

#a

for (i in 1:m){
  p.values[i] <- 1-pnorm(z[i], mean = 0, sd = 1)
  
  reject[i] <- p.values[i]*m < alpha
}

sum(reject)/m

#b

p.values.sorted <- sort(p.values)

i <- 1

while (p.values.sorted[i] <= (i/m)*0.2){
  i <- i+1
}

i

#================

priori <- 0.1

ptautipos <- (0.85*priori)/(0.85*priori + 0.04*(1-priori))

ptautineg <- (0.15*priori)/(0.15*priori + 0.96*(1-priori))

100*ptautipos+900*ptautineg




