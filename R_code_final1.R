                    ### R code for Leu et al 2019 ####
                    ### R version 3.5.0 (2018-04-23) ###
                    
## 1. Install packages
install.packages("lme4")

## 2. Libraries
library(lme4)
                    
## 3. Import data
#setwd("C:/Users/Matthias Leu/Dropbox/ESA paper_2018/Conservation_science_practice/final/data_Rcode")
setwd("C:/Users/mleu/Dropbox/ESA paper_2018/Conservation_science_practice/final/data_Rcode")
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
x <-c(-1.942252658,	-1.852276468,	-1.762300279,	-1.67232409,	-1.582347901,	-1.492371712,	-1.402395523,	-1.312419333,	-1.222443144,	-1.132466955,	-1.042490766,	-0.952514577,	-0.862538388,	-0.772562198,	-0.682586009,	-0.59260982,	-0.502633631,	-0.412657442,	-0.322681252,	-0.232705063,	-0.142728874,	-0.052752685,	0.037223504,	0.127199693,	0.217175883,	0.307152072,	0.397128261,	0.48710445,	0.577080639,	0.667056828,	0.757033018,	0.847009207,	0.936985396,	1.026961585,	1.116937774,	1.206913964,	1.296890153,	1.386866342,	1.476842531,	1.56681872,	1.656794909,	1.746771099,	1.836747288)
x1 <- cbind(1,x)
x2 <- cbind(1, x, x ^ 2) # for quadratic form of year

# Graph number of threats vs. year: quadratic form of year
vcov(TotalThreats.q)
summary(TotalThreats.q)
V <- structure(c(1.055282e-03,6.891181e-05,-2.535045e-04,6.891181e-05,2.991692e-04,-1.309590e-04,-0.0002535045,-0.0001309590, 0.0002514253),.Dim = c(3L, 3L), .Dimnames = list(c("a0", "a1", "a2"), c("a0", "a1", "a2"))) # where V = output from vcov listed by columns of b0,b1, and b2					
beta <- c(1.16619,0.23174,-0.08265) # where c = b0, b1, and b2 from summary (TotalThreats.q)
mu <- x2 %*% beta
e <- sqrt( rowSums(x2 * (x2 %*% V)) ) #prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 3) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 3) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_number_threats_linear_taxa_region_final.csv") # estimates back-transformed in excel: exp (value)

# Graph habitat modification occurrence vs. year: linear form of year
vcov(Hab.l)
summary (Hab.l)
V <- structure(c(0.150861076,	0.002011431,0.002011431,0.007152496), .Dim = c(2L, 2L), .Dimnames = list(c("a0", "a1"), c("a0", "a1"))) # where V = output from vcov listed by columns of b0 and b1					
beta <- c(2.08456, 0.58671) # where c = b0 and b1 from summary(Hab.l)
mu <- x1 %*% beta
e <- sqrt( rowSums(x1 * (x1 %*% V)) ) ## prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 2) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 2) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_hab_taxa_region.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)

# Graph overutilization occurrence vs. year: linear form of year
vcov(Over.l)
summary (Over.l)
V <- structure(c(0.1742535710,0.0001074959,0.0001074959,0.0063162048				
), .Dim = c(2L, 2L), .Dimnames = list(c("a0", "a1"), c("a0", "a1"))) # where V = output from vcov listed by columns of b0 and b1					
beta <- c(-1.27383, -0.19979) # where c = b0 and b1 from summary(Over.l)
mu <- x1 %*% beta
e <- sqrt( rowSums(x1 * (x1 %*% V)) ) ## prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 2) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 2) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output__overutilization_taxa_region.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)

# Graph pollution occurrence vs. year: quadratic form of year
vcov(Poll.q)
summary(Poll.q)
V <- structure(c(0.172867808, 0.003322858,-0.006662736,0.003322858,0.008520027,-0.004007942,-0.006662736,-0.004007942,0.006655236					
), .Dim = c(3L, 3L), .Dimnames = list(c("a0", "a1", "a2"), c("a0", "a1", "a2"))) # where V = output from vcov listed by columns of b0, b1, and b2	
beta <- c(0.21475, 0.57219,-0.58928) # where c = b0, b1, and b2 from summary(Poll.q)
mu <- x2 %*% beta
e <- sqrt( rowSums(x2 * (x2 %*% V)) ) #prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 3) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 3) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_poll_taxa_leadregion.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)

# Graph environmental stochasticity vs. year: linear form of year
vcov(EnvStoch.l)
summary (EnvStoch.l)
V <- structure(c(0.0344612690,-0.0002914772,-0.0002914772,0.0063958442				
), .Dim = c(2L, 2L), .Dimnames = list(c("a0", "a1"), c("a0", "a1"))) # where V = output from vcov listed by columns of b0 and b1					
beta <- c(-0.29170, 1.31542) # where c = b0 and b1 from summary(Over.l)
mu <- x1 %*% beta
e <- sqrt( rowSums(x1 * (x1 %*% V)) ) # prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 2) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 2) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_env_stoch.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)

# Graph species-species interaction occurrence vs. year: quadratic form of year
vcov(SppInter.q)
summary(SppInter.q)
V <- structure(c(0.1119193070,0.0009553301,-0.0041609802,0.0009553301,0.0047349797,-0.0006633695,-0.0041609802,-0.0006633695,0.0037484468				
), .Dim = c(3L, 3L), .Dimnames = list(c("a0", "a1", "a2"), c("a0", "a1", "a2")))# where V = output from vcov listed by columns of b0, b1, and b2 
beta <- c(-0.04328,  0.64312, 0.13892) # where c = b0, b1, and b2 from summary(SppInter.q)
mu <- x2 %*% beta
e <- sqrt( rowSums(x2 * (x2 %*% V)) ) ## prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 3) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 3) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_spsp_taxa_leadregion.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)

# Graph demographic stochasticity occurrence vs. year: quadratic form of year
vcov(DemStoch.q)
summary(DemStoch.q)
V <- structure(c(0.091591616,0.001487748,-0.004649731,0.001487748,0.005070377,-0.001581534,-0.004649731,-0.001581534,0.004034615), .Dim = c(3L, 3L), .Dimnames = list(c("a0", "a1", "a2"), c("a0", "a1", "a2"))) # where V = output from vcov listed by columns of b0, b1, and b2					
beta <- c( 1.10323,  0.62812,  -0.54599) # where c = b0, b1, and b2 from summary(DemStoch.q)
mu <- x2 %*% beta
e <- sqrt( rowSums(x2 * (x2 %*% V)) ) ## prediction standard error
n <- 1547
lo <- mu + e * qt(0.025, n - 3) # lower bound 95% CI
up <- mu - e * qt(0.025, n - 3) # upper bound 95% CI
matplot(x, cbind(mu, lo, up), type = "l", col = 1, lty = c(1,2,2))
tab<-cbind(mu,lo,up)
write.csv(tab, "output_dem_stoch.csv") # estimates back-transformed in excel: exp(value)/1+exp(value)


