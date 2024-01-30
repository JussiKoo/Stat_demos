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

plot(1:100, tn1, "l")
plot(1:100, tn2, "l", col="red")
