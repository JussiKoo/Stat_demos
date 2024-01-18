#T3

pitoisuudet <- c(71.2, 67.5, 77.6, 67.3, 77.7, 67.1)

neglogL <- function(par, data){
  mu <- par[1]
  sigma <- par[2]
  
  neg_log_likelihood <- -sum(dnorm(data, mean = mu, sd = sigma, log = TRUE)) - 4*log(1-pnorm(80, mu, sd=sigma))
  return(neg_log_likelihood)
}

initial_parameters <- c(mean(pitoisuudet), sd(pitoisuudet))

initial_parameters

optim(par=initial_parameters, fn=neglogL, data=pitoisuudet, method="Nelder-Mead")

#==========================================================================================
#T5

data <- read.csv("C:\\kurssit\\stat_demos\\TILA142\\mittalaite.csv")

#Pitoisuudet x, mittaustulokset y

#Negatiivinen log-uskottavuus
neglogL <- function(par, data){
  a <- par[1]
  b <- par[2]
  sigma <- par[3]
  
  #Erotellaan rajan yli menneet havainnot
  data1 <- data[data$ylirajan==0,]
  data2 <- data[data$ylirajan==1,]
  
  v1 <- c()
  
  for (i in 1:length(data1)){
    v1 <- append(v1, dnorm(data1$y[i], a+b*data1$x[i], sigma, log=TRUE))
  }
  
  v2 <- c()
  
  for (i in 1:length(data2)){
    v2 <- append(v2, log(1-pnorm(80, a+b*data2$x[i], sigma, log=FALSE)))
  }
  
  
  logL <- sum(v1) + sum(v2)
  return(-logL)
}

initial_parameters <- c(-16,3.6,12)

result <- optim(par=initial_parameters, fn=neglogL, data=data, method="Nelder-Mead")

ahat <- result$par[1]
bhat <- result$par[2]
sigmahat <- result$par[3]

ahat
bhat
sigmahat







