library(MASS)
library(vcd)
data(quine) 
fit <- goodfit(quine$Days) 
summary(fit) 
rootogram(fit)
Ord_plot(quine$Days)
distplot(quine$Days, type="poisson")
distplot(quine$Days, type="nbinom")

temp <- rpois(1000, 16)
fit <- goodfit(temp) 
rootogram(fit)

mod1 <- glmer(Days~Age+Sex, data=quine, family="poisson")
summary(mod1)
anova(mod1, test="Chisq")

library(AER)
deviance(mod1)/mod1$df.residual
dispersiontest(mod1)

library(pscl)
mod2 <- zeroinfl(Days~Age+Sex, data=quine, dist="poisson")
mod3 <- glm.nb(Days~Age+Sex, data=quine)
mod4 <- zeroinfl(Days~Age+Sex, data=quine, dist="negbin")

AIC(mod1, mod2, mod3, mod4)

mod_nb <- lme4::glmer.nb(Days~scale(Age)+Sex + (1|Eth), data=quine)
summary(mod_nb)

