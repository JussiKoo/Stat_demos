#T5
library(brms)
library(ggplot2)

d <- data.frame(y = c(50, 44, 50, 47, 56), x = 1:5)

priors1 <- c(prior(normal(50,sqrt(10)), class="Intercept"), prior(normal(0,sqrt(10)), class="b"), prior(gamma(2, 0.001), class="sigma"))
priors2 <- c(prior(normal(48.3, 1), class="Intercept"), prior(normal(0,1), class="b"), prior(gamma(2, 0.001), class="sigma"))

fit1 <- brm(y ~ I(x - mean(x)), data = d, prior = priors1)
fit2 <- brm(y ~ I(x - mean(x)), data = d, prior = priors2)

#Posterioriennustejakauman odotusarvoja
draw1 <- posterior_epred(fit1)
draw2 <- posterior_epred(fit2)

summary_fun <- function(x) {
  c(
    Estimate = mean(x), 
    Est.Error = sd(x), 
    Q2.5 = unname(quantile(x, 0.025)),
    Q97.5 = unname(quantile(x, 0.975))
  )
}

#Keskiarvoistetaan kolumnien yli ja lasketaan kvantiilit
summary1 <- as.data.frame(t(apply(draw1, 2, summary_fun)))
summary2 <- as.data.frame(t(apply(draw2, 2, summary_fun)))
summary1$xcentered <- d$x-mean(d$x)
summary2$xcentered <- d$x-mean(d$x)

ggplot(summary1, aes(y=Estimate, x=xcentered)) +
  geom_ribbon(mapping=aes(ymin = Q2.5, ymax = Q97.5), fill="red", alpha=0.25) +
  geom_ribbon(mapping=aes(ymin = Q2.5, ymax = Q97.5), summary2, fill="blue", alpha=0.25)+
  geom_line(col="red")+
  geom_line(summary2, mapping=aes(y=Estimate, x=xcentered), col="blue")

draw_small = posterior_epred(fit1, ndraws=50)



#T6

beetles <- read.table("http://users.jyu.fi/~santikka/bayes1/data/beetles.txt")

fitbeetle <- brm(alive | trials(total) ~ I(treatment-mean(treatment)), data=beetles, family=binomial("logit"))
fitbeetle

#Betahat 95% posterioriväli on [-0.15, -0.06]. 
#Voisi todeta, että annostuksen kasvattaminen vähentää hieman selviytymisen todennäköisyyttä

draw <- as_draws_df(fitbeetle)

#Oikein!
mean(exp(draw$Intercept))
mean(exp(draw$b_ItreatmentMmeantreatment))

#Väärin! Jensenin epäyhtälö! Ensin muunnos kaikkiin posteriorinäytteisiin
#ja niistä lasketaan keskiarvo
exp(mean(draw$b_ItreatmentMmeantreatment)) 
exp(mean(draw$Intercept))

#Torakalle 1 on annettu 1 mikrogramma/ml enemmän myrkkyä kuin torakalle 2
#OR näiden välillä on
exp(betahat)

#Odds torakan selviytymiselle kun myrkkyannos on keskiarvo 4.36 migrog/ml
mean(beetles$treatment)
exp(alphahat)

conditional_effects(fitbeetle)
