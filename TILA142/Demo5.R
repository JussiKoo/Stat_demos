#T3

freq <- c(315, 108, 101, 32)

thetahat <- freq/sum(freq)

thetanull <- c(9/16, 3/16, 3/16, 1/16)

#Testataan esim. uskottavuusosamäärän testillä.

dmultinom(freq, prob=thetanull)/dmultinom(freq, prob=thetahat)

D <- -2*(dmultinom(freq, prob=thetanull, log=TRUE) - dmultinom(freq, prob=thetahat, log=TRUE))

1-pchisq(D, df=1)
