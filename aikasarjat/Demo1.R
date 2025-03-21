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
