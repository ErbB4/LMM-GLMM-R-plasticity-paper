library(lme4)
library(MuMIn)

#############24h##############
#load data
data = read.csv("soma_size_24h.csv")
#check data
str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalNo)

options(na.action = "na.fail")


#select model
mod1 <-lmer(Area ~ Sham * Ipsi + (1|AnimalNo), data = data)
mod2 <-lm(Area ~ Sham * Ipsi, data = data)
AIC(mod1,mod2) #the random effects structure in the mod1 is optimal


dd1 <- dredge(mod1)
b1 <- get.models(dd1, 1)[[1]]
confint(b1) # not significant

#############48h##############
data = read.csv("soma_size_48h.csv")
#check data
str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalNo)

options(na.action = "na.fail")


#select model
mod1 <-lmer(Area ~ Sham * Ipsi + (1|AnimalNo), data = data)
mod2 <-lm(Area ~ Sham * Ipsi, data = data)
AIC(mod1,mod2) #mod1 contains the optimal random effects structure

#get the fixed effects structure

dd1 <- dredge(mod1)
b1 <- get.models(dd1, 1)[[1]]
confint(b1) # not significant

