#thetahat on h(theta) maksimi eli derivaatan h'(theta) nollakohta.

y <- c(
  3.24, 1.42, 6.83, 3.60, 4.30, 0.81, 2.83, 1.01, 3.23, 5.99, 2.70, 2.09,
  4.88, 5.31, 0.44, 1.98, 0.19, 4.26, 2.39, 2.04, 1.29, 1.01, 2.57, 0.03
)

#thetahat ratkeaa selvittämällä toisen asteen polynomin juuret.

a <- -sum(y)
b <- length(y)-5
c <- 3/2

#vain positiivinen tulos käy sillä theta>0
thetahat <- (-b - sqrt(b^2 - 4*a*c))/(2*a)

#V = -h''(thetahat)^-1

V <- 1/((length(y)-5)/thetahat^2 + 3/thetahat^3)

#p(theta|y) ~ N(thetahat, V)

thetahat
V

#Posteriorin approksimaatio on siis normaalijakauma 
#odotusarvolla ~0,36 ja varianssilla ~ 0,0047
