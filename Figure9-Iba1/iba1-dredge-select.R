library(lme4)
library(MuMIn)

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


data = read.csv("iba1.csv")
data$Time <- as.factor(data$Time)
data$Sham <- as.factor(data$Sham)
data$Ipsi <- as.factor(data$Ipsi) 
data$Distance2 <- as.factor(data$Distance2)
#data$Distance1 <- as.factor(data$Distance1)
data$AnimalNo <- as.factor(data$AnimalNo)

time1 <- as.data.frame(data[data$Time=="1",])
time2 <- as.data.frame(data[data$Time=="2",])
time3 <- as.data.frame(data[data$Time=="3",])

options(na.action = "na.fail")

#Time1
mod1 = glm(Counting ~ Sham*Ipsi*Distance1, family=poisson,data = time1)
mod2 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo/Distance1), family=poisson,data = time1)
mod3 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1+Distance1|AnimalNo), family=poisson,data = time1)
mod4 = glmer(Counting ~ Sham*Ipsi + (1|AnimalNo) + (1|Distance1), family=poisson,data = time1)
mod5 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo), family=poisson,data = time1)

AIC(mod1,mod2,mod3,mod4,mod5)

dd2 <- dredge(mod2)
b2 <- get.models(dd2, 1)[[1]]
overdisp_fun(b2)
summary(b2)

#re-fit with negative binominal distribution
library(MASS)
mod1 = glm.nb(Counting ~ Sham*Ipsi*Distance1, data = time1)
mod2 = glmer.nb(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo/Distance1),data = time1)
mod3 = glmer.nb(Counting ~ Sham*Ipsi*Distance1 + (1+Distance1|AnimalNo),data = time1)
mod4 = glmer.nb(Counting ~ Sham*Ipsi + (1|AnimalNo) + (1|Distance1),data = time1)
mod5 = glmer.nb(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo),data = time1)

AIC(mod1,mod2,mod3,mod4,mod5)
dd2 <- dredge(mod4)
b2 <- get.models(dd2, 1)[[1]]
overdisp_fun(b2)
summary(b2)

#Time2
mod1 = glm(Counting ~ Sham*Ipsi*Distance1, family=poisson,data = time2)
mod2 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo/Distance1), family=poisson,data = time2)
mod3 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1+Distance1|AnimalNo), family=poisson,data = time2)
mod4 = glmer(Counting ~ Sham*Ipsi + (1|AnimalNo) + (1|Distance1), family=poisson,data = time2)
mod5 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo), family=poisson,data = time2)

AIC(mod1,mod2,mod3,mod4,mod5)

dd2 <- dredge(mod2)
b2 <- get.models(dd2, 1)[[1]]
overdisp_fun(b2)
summary(b2)
#re-fit with negative binominal distribution
mod1 = glm.nb(Counting ~ Sham*Ipsi*Distance1, data = time2)
mod2 = glmer.nb(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo/Distance1),data = time2)
mod3 = glmer.nb(Counting ~ Sham*Ipsi*Distance1 + (1+Distance1|AnimalNo),data = time2)
mod4 = glmer.nb(Counting ~ Sham*Ipsi + (1|AnimalNo) + (1|Distance1),data = time2)
mod5 = glmer.nb(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo),data = time2)

AIC(mod1,mod2,mod3,mod4,mod5)
dd2 <- dredge(mod1)
b2 <- get.models(dd2, 1)[[1]]
overdisp_fun(b2)
summary(b2)

#Time3
mod1 = glm(Counting ~ Sham*Ipsi*Distance1, family=poisson,data = time3)
mod2 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo/Distance1), family=poisson,data = time3)
mod3 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1+Distance1|AnimalNo), family=poisson,data = time3)
mod4 = glmer(Counting ~ Sham*Ipsi + (1|AnimalNo) + (1|Distance1), family=poisson,data = time3)
mod5 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo), family=poisson,data = time3)

AIC(mod1,mod2,mod3,mod4,mod5)

dd2 <- dredge(mod2)
b2 <- get.models(dd2, 1)[[1]]
overdisp_fun(b2)
summary(b2)
plot(b2)
