#Jussi Kauppinen
#2891068547

#Libraries

library(glmmTMB)
library(pscl)
library(ggplot2)
library(lattice)
library(mltest)
library(ResourceSelection)
library(xtable)

#===============================================================================
#Task 2
#===============================================================================

salam <- Salamanders
attach(salam)

plot(table(count), ylab="frequency")

#Mining's effect on the observed number of salamanders
plot(count ~ mined)

#Covers effect on the observed number of salamanders
plot(count ~ cover)

#Regular Poisson model
fit <- glm(count ~ mined + cover, data = salam, family="poisson")
summary(fit)

#Hurdle model
fith <- hurdle(count ~ mined + cover, data = salam,
               dist="poisson", zero.dist="binomial", link="logit")
summary(fith)

#AIC for both models
AIC(fit, fith)

#Compare log-likelihoods
logLik(fit)
logLik(fith)

#Expected number of zero-observations from both models and the actual number.

#Poisson
sum(dpois(0, fitted(fit)))

#Hurdle
sum(predict(fith, type="prob")[,1])

#Actual
sum(as.numeric(salam$count==0))

b1 <- exp(coef(fit))
b1

b2 <- exp(coef(fith))
b2

#Expected number of observed salamanders when the site was affected by
#mountain top removal coal mining and scaled number of cover objects is
#zero.

#Poisson
b1[1]

#Hurdle
b2[1]

mean(subset(salam, salam$mined=="yes")$count)

#Percentual change in observed number of salamanders in sites where other is
#affected by mining and other is not.

#Poisson
(b1[2]-1)*100

#Hurdle
(b2[2]-1)*100

#Percentual change in number of observed salamanders between sites with one 
#unit difference in scaled number of cover objects.

#Poisson
(b1[3]-1)*100

#Hurdle
(b2[3]-1)*100

#Odds to observe any salamanders in a site that was affected by mining
#and scaled number of cover objects is zero.
b2[4]

#OR between site that is affected by mining and site that isn't.
b2[5]

#OR between sites with one unit difference in scaled number of cover objects.
b2[6]

#===============================================================================
#Task 3
#===============================================================================

discharge <- read.table("discharge.txt", header=TRUE, sep=";")

#Shift binary values to 0 and 1
discharge$death <- discharge$death - 1
discharge$gender <- discharge$gender - 1
discharge$inh_inj <- discharge$inh_inj - 1
discharge$flame <- discharge$flame - 1
discharge$race <- discharge$race - 1
discharge$gender <- factor(discharge$gender, levels=c(0,1), 
                           labels=c("Female", "Male"))
discharge$inh_inj <- factor(discharge$inh_inj, levels=c(0,1), 
                            labels=c("No", "Yes"))
discharge$flame <- factor(discharge$flame, levels=c(0,1), 
                          labels=c("No", "Yes"))
discharge$race <- factor(discharge$race, levels=c(0,1), 
                         labels=c("Non-white", "White"))

#Convert age and tbsa to numeric values
discharge$age <- as.numeric(gsub(",", ".", discharge$age))
discharge$tbsa <- as.numeric(gsub(",", ".", discharge$tbsa))

set.seed(8547)

#sample indexes
s <- sample(nrow(discharge), 850)

#Split discharge data into modeling data and test data
data <- discharge[s,]
test <- discharge[-s,]



breaks <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)

labels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90")

#Age-groups for visualization
data$age_group <- cut(data$age, breaks = breaks, labels = labels, right = FALSE)

#logit-function to investigate linearity of the effects
logit <- function(data){
  log(mean(data)/(1-mean(data)))
}

#Compute ratio of deaths vs cases for each age-group.
dratioage <- aggregate(data$death, by=list(data$age_group), FUN=logit)

ggplot(data=dratioage, aes(x=Group.1, y=x))+
  geom_point()+
  xlab("Age")+
  ylab("log-odds of dying")+
  theme_bw()

breaks <- seq(0,100, 10)

labels <- c("0-10", "10-20", "20-30", "30-40",
            "40-50", "50-60", "60-70", "70-80",
            "80-90", "90-100")

data$tbsa_group <- cut(data$tbsa, breaks = breaks, labels=labels, right = FALSE)

dratioburn <- aggregate(data$death, by=list(data$tbsa_group), FUN=logit)

ggplot(data=dratioburn, aes(x=Group.1, y=x))+
  geom_point()+
  xlab("Total burn area")+
  ylab("log-odds of dying")+
  theme_bw()

dratioinh <- aggregate(data$death, by=list(data$inh_inj), FUN=mean)

ggplot(data=dratioinh, aes(x=as.factor(Group.1), y=x))+
  geom_bar(stat="identity")+
  xlab("Burn involved inhalation injury")+
  ylab("Mortality rate")+
  ylim(0,1)

dratioflame <- aggregate(data$death, by=list(data$flame), FUN=mean)

ggplot(data=dratioflame, aes(x=as.factor(Group.1), y=x))+
  geom_bar(stat="identity")+
  xlab("Flame involved in burn injury")+
  ylab("Mortality rate")+
  ylim(0,1)

ggplot(data=data, aes(x=inh_inj, y=tbsa))+
  geom_boxplot()

ggplot(data=data, aes(x=flame, y=tbsa))+
  geom_boxplot()

dratiogender <- aggregate(data$death, by=list(data$gender), FUN=mean)

ggplot(data=dratiogender, aes(x=as.factor(Group.1), y=x))+
  geom_bar(stat="identity")+
  xlab("Gender")+
  ylab("Mortality rate")+
  ylim(0,1)

dratiorace <- aggregate(data$death, by=list(data$race), FUN=mean)

ggplot(data=dratiorace, aes(x=as.factor(Group.1), y=x))+
  geom_bar(stat="identity")+
  xlab("Race")+
  ylab("Mortality rate")+
  ylim(0,1)

fitall <- glm(death ~ age + gender + race + tbsa + inh_inj + flame, data=data,
              family="binomial")
summary(fitall)

fit1 <- glm(death ~ I(age-30)*inh_inj + I(tbsa-15)*inh_inj, data=data,
            family="binomial")

fit2 <- glm(death ~ I(age-30) + I(tbsa-15)*inh_inj, data=data,
            family="binomial")

fit3 <- glm(death ~ I(age-30)*inh_inj + I(tbsa-15), data=data,
            family="binomial")

fit4 <- glm(death ~ I(age-30) + I(tbsa-15), data=data,
            family="binomial")

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)

hoslem.test(data$death, fitted(fit1))

#Take relevant predictors
test <- data.frame(death = test$death, age = test$age, tbsa = test$tbsa, inh_inj = test$inh_inj)

#Predictions
pred <- predict(fit1, newdata = test, se.fit=TRUE, type="response")

test <- cbind(test, probs = round(pred$fit,2))

test$preds <- as.numeric(test$probs > 0.5)

#Ratio of correct predictions
sum(test$death == test$preds)/nrow(test)

expb <- exp(coef(fit1))
expb

#OR between two patients with same tbsa and no inhalation injury with age
#difference of one year.
expb[2]

#OR between two patients with same tbsa and inhalation injury with age
#difference of one year.
expb[2]+expb[5]

#OR between patients of same age and no inhalation injury with one percentage
#point difference in total burnt surface area
expb[4]

#OR between patients of same age and inhalation injury with one percentage
#point difference in total burnt surface area
expb[4]+expb[6]

#OR between patients of same age and with same burnt surface area. Other has
#inhalation injury and other has not.
expb[3]

exp(confint(fit1))

b <- coef(fit1)
b

invlogit <- function(x){
  plogis(x)
}

ages <- seq(-20, 60, 10)

probs1 <- invlogit(b[1] + ages*b[2])
probs2 <- invlogit(b[1] + ages*(b[2]+b[5]) + b[3])

d1 <- data.frame(age = ages+30, no_inh_inj = probs1, inh_inj = probs2)

#Probabilities to die in a hospital for different ages with tbsa of 15%.
d1

tbsas <- seq(-5, 75, 10)

probs3 <- invlogit(b[1] + tbsas*b[4])
probs4 <- invlogit(b[1] + tbsas*(b[4]+b[6]) + b[3])

d2 <- data.frame(tbsa = tbsas+15, no_inh_inj = probs3, inh_inj = probs4)

#Probabilities for 30-year old patients with different tbsa to die in a 
#hospital.
d2
