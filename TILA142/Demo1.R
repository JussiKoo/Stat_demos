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

data <- read.csv("C:\\kurssit\\TILA142\\mittalaite.csv")

#Pitoisuudet x, mittaustulokset y

neglogL <- function(par, data){
  a <- par[1]
  b <- par[2]
  sigma <- par[3]
  
  data1 <- data[data$ylirajan==0,]
  data2 <- data[data$ylirajan==1,]
  
  v1 <- c()
  
  for (i in 1:length(data1)){
    v1 <- append(v1, dnorm(data1$y[i], a+b*data1$x[i], sigma, log=TRUE))
  }
  
  v2 <- c()

  for (i in 1:length(data2)){
    v2 <- append(v2, log(1-pnorm(80, a+b*data2$x[i], sigma, log=TRUE)))
  }
  
  
  neg_log_likelihood <- -sum(v1) - sum(v2)
  #Tässä korjaa tuo a+b*data1$x, pitää olla yksi arvo?
  return(neg_log_likelihood)
}

plot(data$x, data$y)
abline(a=-167, b=3.6)

initial_parameters <- c(25, 0.6, 5)

optim(par=initial_parameters, fn=neglogL, data=data, method="Nelder-Mead")







