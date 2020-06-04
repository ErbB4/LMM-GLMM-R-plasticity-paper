library(lme4)
library(MuMIn)

data = read.csv("gfap.csv")
options(na.action = "na.fail")
data$Time <- as.factor(data$Time) 
data$Sham <- as.factor(data$Sham) 
data$Ipsi <- as.factor(data$Ipsi) 
data$Distance2 <- as.factor(data$Distance2)
#data$Distance1 <- as.factor(data$Distance1)
data$AnimalNo <- as.factor(data$AnimalNo)


#Time1
time1 <- as.data.frame(data[data$Time=="1",])

#select the good random effects structure

mod1 <- lm(Intensity~Sham*Ipsi*Distance1,data=time1)
mod2 <- lmer(Intensity~Sham*Ipsi*Distance1+(1|AnimalNo),REML=TRUE,data=time1)
mod3 <- lmer(Intensity~Sham*Ipsi*Distance1+(1+Distance1|AnimalNo),control=lmerControl(optimizer ="Nelder_Mead"),REML=TRUE,data=time1)
mod4 <- lmer(Intensity~Sham*Ipsi*Distance1+(1|AnimalNo/Distance1),control=lmerControl(optimizer ="Nelder_Mead"),REML=TRUE,data=time1)
mod5 <- lmer(Intensity~Sham*Ipsi*Distance1+(1|AnimalNo)+(1|Distance1),REML=TRUE,data=time1)


AIC(mod1,mod2,mod3,mod4,mod5) #mod3 is the best
summary(mod3)

#select the best fixted effects structure

dd3 <- dredge(mod2)
b3 <- get.models(dd3, 1)[[1]]
summary(b3)
confint(b3,level=0.95)
confint(b3,level=0.99) 
confint(b3,level=0.999) 


#Time2
time2 <- as.data.frame(data[data$Time=="2",])

#select the good random effects structure

mod1 <- lm(Intensity~Sham*Ipsi*Distance1,data=time2)
mod2 <- lmer(Intensity~Sham*Ipsi*Distance1+(1|AnimalNo),REML=TRUE,data=time2)
mod3 <- lmer(Intensity~Sham*Ipsi*Distance1+(1+Distance1|AnimalNo),control=lmerControl(optimizer ="Nelder_Mead"),REML=TRUE,data=time2)
mod4 <- lmer(Intensity~Sham*Ipsi*Distance1+(1|AnimalNo/Distance1),control=lmerControl(optimizer ="Nelder_Mead"),REML=TRUE,data=time2)
mod5 <- lmer(Intensity~Sham*Ipsi*Distance1+(1|AnimalNo)+(1|Distance1),REML=TRUE,data=time2)


AIC(mod1,mod2,mod3,mod4,mod5) #mod2 is the best
summary(mod2)

#select the best fixted effects structure

dd4 <- dredge(mod2)
b4 <- get.models(dd4, 1)[[1]]
summary(b4)
confint(b4,level=0.95)
confint(b4,level=0.99)
confint(b4,level=0.999)


#Time3
time3 <- as.data.frame(data[data$Time=="3",])

#select the good random effects structure

mod1 <- lm(Intensity~Sham*Ipsi*Distance1,data=time3)
mod2 <- lmer(Intensity~Sham*Ipsi*Distance1+(1|AnimalNo),REML=TRUE,data=time3)
mod3 <- lmer(Intensity~Sham*Ipsi*Distance1+(1+Distance1|AnimalNo),control=lmerControl(optimizer ="Nelder_Mead"),REML=TRUE,data=time3)
mod4 <- lmer(Intensity~Sham*Ipsi*Distance1+(1|AnimalNo/Distance1),control=lmerControl(optimizer ="Nelder_Mead"),REML=TRUE,data=time3)
mod5 <- lmer(Intensity~Sham*Ipsi*Distance1+(1|AnimalNo)+(1|Distance1),REML=TRUE,data=time3)


AIC(mod1,mod2,mod3,mod4,mod5) #mod2 is the best
summary(mod2)

#select the best fixted effects structure

dd2 <- dredge(mod2)
b2 <- get.models(dd2, 1)[[1]]
summary(b2)
confint(b2,level=0.95) 
confint(b2,level=0.99) 
confint(b2,level=0.999) 
