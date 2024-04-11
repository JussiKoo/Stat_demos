library(nlme) # library for gls and the rats data
data(BodyWeight)
BW <- BodyWeight #shorter name
head(BW, 8)

library(lattice)
xyplot(weight~Time, group= Rat, data=BW, type="b",as.table=T)

aggregate(weight ~ Diet + Time, data=BW, mean)

model1 <- gls(weight ~ Time*Diet, data=BW, method = "ML")
summary(model1)

plot(residuals(model1) ~ fitted(model1), col = BW$Diet)

model2 <- gls(weight ~ Time*Diet, data=BW, correlation = corCompSymm(form = ~ 1 |Rat),  method = "ML")

summary(model2)

coef(model1$modelStruct)

pisa <- read.table("http://users.jyu.fi/~knordhau/GLM2/pisafull.txt", header = TRUE)
pisa[1:5,]

fitML <- gls(sciescore ~ matem + ESCS + motiv + gender + region + urban,
             correlation = corCompSymm(form = ~ 1 | SCHOOLID),
             data = pisa, method = "ML")

summary(fitML)

fitREML <- gls(sciescore ~ matem + ESCS + motiv + gender + region + urban,
             correlation = corCompSymm(form = ~ 1 | SCHOOLID),
             data = pisa, method = "REML")

summary(fitREML)


