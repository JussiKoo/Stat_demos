################################################################################

#Excercise 1

data <- read.csv("C:\\kurssit\\TILA141\\phone_talking.csv")
n <- nrow(data)
k <- sum(data$talking)

thetahat <- k/n

#Normaalijakauma-approksimaatio
thetaci <- c(thetahat + qnorm(0.025,0,1)/sqrt(n), thetahat + qnorm(0.975,0,1)/sqrt(n))
thetaci



#Parametriton bootstrap
nsim <- 1000
thetahatsim <- rep(NA,nsim)

for(i in 1:nsim)
{  
  datasim <- sample(data$talking, replace = TRUE)
  
  thetahatsim[i] <- sum(datasim)/length(datasim)
}

thetaci <- c( quantile(thetahatsim,0.025), quantile(thetahatsim,0.975) )

thetaci

################################################################################

#Excercise 2

library(mnormt)

mu <- c(2,2,1,1)

cov <- cbind(c(2,1.6,0.4,0.4),c(1.6,2,0.4,0.4),c(0.4,0.4,1,0.8),c(0.4,0.4,0.8,1))
S <- 6*cov/8

x <- rmt(n = 10000, mean = mu, S = S, df = 8)
y <- rep(NA, 10000)

for (i in 1:10000) 
{
  y[i] <- ((x[i,][1] + x[i,][2])^2) / (1 + abs(x[i,][3]) + abs(x[i,][4]))
}

nsim <- 10000
means <- rep(NA, nsim)

#a

for (i in 1:nsim)
{
  sy <- sample(y, 10000, replace=TRUE) #Tehdään uusio-otos

  means[i] <- mean(sy) #Lasketaan odotusarvo otoksesta
}

sd(means)

#b

#x edelleen otos t-jakautuneista satunnaisvektoreista.
#Oletetaan, että x on normaalijakautunut.

#Määritetään ensin estimoitu odotusarvovektori suurimman uskottavuuden 
#menetelmällä
muhat <- c(mean(x[,1]), mean(x[,2]), mean(x[,3]), mean(x[,4]))

#Määritetään keskivirheet suurimman uskottavuuden menetelmällä.
sigmahat2 <- cov(x)

library(MASS)

#simuloidaan tästä normaalijakaumasta uusia otoksia ja lasketaan niissä
#Halutun satunnaismuuttujan odotusarvo.
for (i in 1:nsim)
{
   sx <- mvrnorm(10000, mu = muhat, Sigma=sqrt(sigmahat2))
   for (j in 1:10000) 
   {
     y[j] <- ((sx[j,][1] + sx[j,][2])^2) / (1 + abs(sx[j,][3]) + abs(sx[j,][4]))
   }
   means[i] <- mean(y)
}

sd(means)

################################################################################

#Excercise 3

library(boot)

x <- rnorm(20, 0, 1)
nsim <- 1000

meanfun <- function(d,ind)
{
  return(mean(d[ind]))  
} 

meanboot <- boot(data=x, statistic=meanfun, R=nsim)
meanci <- boot.ci(meanboot, type = c("norm", "basic", "perc", "bca"))

meanci

################################################################################

#Excercise 4

Hdist <- function(n, theta, nsim)
{
  Hs <- rep(NA,nsim)
  
  for (j in 1:nsim)
  {
    guesses <- rgeom(n, theta) #Simuloidaan ihmisten arvauksia
    sguesses <- sort(guesses) #Järjestetään arvaukset
    H <- -1
    nsame <- 1
    
    if (length(sguesses) == 1)
    {
      H <- sguesses[1]
      Hs[j] <- H
      break
    }
    
    #Etstitään voittava arvaus
    for (i in 1:(length(sguesses)-1))
    {
      if (sguesses[i] != sguesses[i+1])
      {
        if (nsame == 1) 
        {
          H <- sguesses[i]
          break
        }
        nsame <- 1
      }
      else nsame = nsame+1
    }
    
    Hs[j] <- H
  }
  
  #Otetaan pois -1 H-arvot eli kun kukaan ei voittanut.
  Hs <- Hs[! Hs %in% -1]
  
  lambdahat <- sum(Hs)/length(Hs)
  
  plot(table(Hs)/length(Hs), xlab="Occurences of different H", ylab="Relative frequency")
  #lines(1:20, dpois(1:20, lambda=lambdahat), col="red") #Jakauman sovittaminen, mikä?
}

#Testausta

Hdist(100, 0.4, 10000)

simH <- function(n, theta, m) {
  H <- rep(NA,m)
  for(j in 1:m) {
    x <- 1 + rgeom(n,theta)
    xtable <- table(x)
    xtable <- xtable[xtable == 1] # arvattu kerran
    if(length(xtable) > 0) {
      # pienin kerran arvattu
      H[j] <- min(as.numeric(names(xtable)))
    }
  }
  return(H)
}
simu <- simH(100,0.1,10000)
table(simu)


################################################################################

#Excercise 5

################################################################################