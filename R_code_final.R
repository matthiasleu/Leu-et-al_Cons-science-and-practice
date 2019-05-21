                    ### R code for Leu et al 2019 ####
                    ### R version 3.5.0 (2018-04-23) ###
                    
## 1. Install packages
install.packages("lme4")

## 2. Libraries
library(lme4)
                    
## 3. Import data
dat=read.csv("DataLeuEtAl.csv", header = TRUE)

## 4. Data preparation
dat$year75<-(dat$year-1975) # Relate to 1975
dat$yearcs<-scale(dat$year75) # Center and standardize data
dat$yearcs2<-(dat$yearcs^2) # Quadratic form
dat$yearln<-log(dat$year75+1) # Pseudothreshold form
dat$lead_region <- factor(dat$lead_region) # Lead region as factor

## 5. Model structure for threat vs. year: r=random, l=linear, q=quadratic, log=ln; AIC values summarized in Appendix S4
# Check if poisson is appropriate error structure
mean(dat$total)
var(dat$total) #mean & variance not equal, therefore used negative binomial model structure

summary(TotalThreats.r <- glmer.nb(total ~ 1 +(1|taxa), data = dat, control = glmerControl(optimizer = 'bobyqa')))

summary(TotalThreats.l <- glmer.nb(total ~ yearcs + (1|taxa), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(TotalThreats.q <- glmer.nb(total ~ yearcs+yearcs2 + (1|taxa), data = dat,  control = glmerControl(optimizer = 'bobyqa')))

summary(TotalThreats.ln <- glmer.nb(total ~ yearln+(1|taxa), data = dat,  control = glmerControl(optimizer = 'bobyqa')))

# Check distribution of residuals: Models with log of year was not converging. Used model with quadratic form of year because this model had lowest AIC.
resid<-resid(TotalThreats.q, type = 'response')
plot(resid~dat$yearcs)

## 6. Model structure for threat probability vs. year: r=random, l=linear, q=quadratic, log=ln; AIC values summarized in Appendix S6

# Habitat modification
summary(Hab.r <- glmer(hab~1+(1|taxa)+(1|lead_region), binomial(link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(Hab.l <- glmer(hab~yearcs+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(Hab.q <- glmer(hab~yearcs+yearcs2+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(Hab.ln <- glmer(hab~yearln+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

# Overutilization
summary(Over.r <- glmer(over~1+(1|taxa)+(1|lead_region), binomial(link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(Over.l <- glmer(over~yearcs+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat,control = glmerControl(optimizer = 'bobyqa')))

summary (Over.q <- glmer(over~yearcs+yearcs2+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(Over.ln <- glmer(over~yearln+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

# Pollution
summary(Poll.r <- glmer(poll~1+(1|taxa)+(1|lead_region), binomial(link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(Poll.l <- glmer(poll~yearcs+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat,control = glmerControl(optimizer = 'bobyqa')))

summary(Poll.q <- glmer(poll~yearcs+yearcs2+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(Poll.ln <- glmer(poll~yearln+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

# Environmental stochasticity
summary(EnvStoch.r <- glmer(env~1+(1|taxa)+(1|lead_region), binomial(link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(EnvStoch.l <- glmer(env~yearcs+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat,control = glmerControl(optimizer = 'bobyqa')))

summary(EnvStoch.q <- glmer(env~yearcs+yearcs2+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(EnvStoch.ln <- glmer(env~yearln+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

# Species-species interactions
summary(SppInter.r <- glmer(spsp~1+(1|taxa)+(1|lead_region), binomial(link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(SppInter.l <- glmer(spsp~yearcs+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat,control = glmerControl(optimizer = 'bobyqa')))

summary(SppInter.q <- glmer(spsp~yearcs+yearcs2+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(SppInter.ln <- glmer(spsp~yearln+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

# Demographic stochasticity
summary(DemStoch.r <- glmer(demo~1+(1|taxa)+(1|lead_region), binomial(link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(DemStoch.l <- glmer(demo~yearcs+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat,control = glmerControl(optimizer = 'bobyqa')))

summary(DemStoch.q <- glmer(demo~yearcs+yearcs2+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

summary(DemStoch.ln <- glmer(demo~yearln+(1|taxa)+(1|lead_region), family = binomial (link = "logit"), data=dat, control = glmerControl(optimizer = 'bobyqa')))

## Graphs for models with lowest AIC values: used variance-covariance matrix to estimate 95% confidence intervals

# Vector for centered and standardized year values in graph
x <- unique(sort(as.vector(scale(dat$year75))))
x1 <- cbind(1,x)
x2 <- cbind(1, x, x ^ 2) # for quadratic form of year

# Graph number of threats vs. year: quadratic form of year
V <- vcov(TotalThreats.q) # where V = output from vcov listed by columns of b0,b1, and b2	
TotalThreats.q.summary <- summary(TotalThreats.q)
beta <- c(TotalThreats.q.summary$coefficients[1:3])
mu <- x2 %*% beta
e <- sqrt( rowSums(x2 * (x2 %*% V)) ) #prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 3) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 3) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_number_threats_linear_taxa_region_final.csv") # estimates back-transformed in excel: exp (value)

# Graph habitat modification occurrence vs. year: linear form of year
V <- vcov(Hab.l) # where V = output from vcov listed by columns of b0 and b1
Hab.l.summary <- summary(Hab.l)
beta <- c(Hab.l.summary$coefficients[1:2])
mu <- x1 %*% beta
e <- sqrt( rowSums(x1 * (x1 %*% V)) ) ## prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 2) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 2) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_hab_taxa_region.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)

# Graph overutilization occurrence vs. year: linear form of year
V <- vcov(Over.l) # where V = output from vcov listed by columns of b0 and b1
Over.l.summary <- summary (Over.l)
beta <- c(Over.l.summary$coefficients[1:2])
mu <- x1 %*% beta
e <- sqrt( rowSums(x1 * (x1 %*% V)) ) ## prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 2) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 2) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output__overutilization_taxa_region.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)

# Graph pollution occurrence vs. year: quadratic form of year
V <- vcov(Poll.q) # where V = output from vcov listed by columns of b0,b1, and b2	
Poll.q.summary <- summary(Poll.q)
beta <- c(Poll.q.summary$coefficients[1:3]) # where c = b0, b1, and b2 from summary(Poll.q)
mu <- x2 %*% beta
e <- sqrt( rowSums(x2 * (x2 %*% V)) ) #prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 3) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 3) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_poll_taxa_leadregion.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)

# Graph environmental stochasticity vs. year: linear form of year
V <- vcov(EnvStoch.l) # where V = output from vcov listed by columns of b0 and b1
EnvStoch.l.summary <- summary (EnvStoch.l)
beta <- c(EnvStoch.l.summary$coefficients[1:2])
mu <- x1 %*% beta
e <- sqrt( rowSums(x1 * (x1 %*% V)) ) ## prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 2) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 2) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_env_stoch.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)

# Graph species-species interaction occurrence vs. year: quadratic form of year
V <- vcov(SppInter.q) # where V = output from vcov listed by columns of b0,b1, and b2	
SppInter.q.summary <- summary(SppInter.q)
beta <- c(SppInter.q.summary$coefficients[1:3]) # where c = b0, b1, and b2 from summary(SppInter.q)
mu <- x2 %*% beta
e <- sqrt( rowSums(x2 * (x2 %*% V)) ) #prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 3) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 3) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_spsp_taxa_leadregion.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)

# Graph demographic stochasticity occurrence vs. year: quadratic form of year
V <- vcov(DemStoch.q) # where V = output from vcov listed by columns of b0,b1, and b2	
DemStoch.q.summary <- summary(DemStoch.q)
beta <- c(DemStoch.q.summary$coefficients[1:3]) # where c = b0, b1, and b2 from summary(DemStoch.q)
mu <- x2 %*% beta
e <- sqrt( rowSums(x2 * (x2 %*% V)) ) #prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 3) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 3) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_dem_stoch.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)


