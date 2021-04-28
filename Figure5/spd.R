library(lme4)

###########24h, overall##########
data = read.csv("dendrite_spd_24h.csv")

str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)

#select model, with fixed random structure

m.24h.int  <- lmer(spine_density ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.add  <- lmer(spine_density ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.sham <- lmer(spine_density ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.24h.ipsi <- lmer(spine_density ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.null  = lmer(spine_density ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.24h.int,m.24h.add,m.24h.sham,m.24h.ipsi,m.24h.null)# m.24h.int is the best

plot(m.24h.sham)
summary(m.24h.sham)
confint(m.24h.sham,level=0.95) #significant at 95%

###############24h,apical##########

data = read.csv("dendrite_spd_24h.csv")
data = as.data.frame(data[data$apical=="apical",])
str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)

#select model

m.24h.int  <- lmer(spine_density ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.add  <- lmer(spine_density ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.sham <- lmer(spine_density ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.24h.ipsi <- lmer(spine_density ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.null  = lmer(spine_density ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.24h.int,m.24h.add,m.24h.sham,m.24h.ipsi,m.24h.null)# again: the interaction should be kept

plot(m.24h.int)
summary(m.24h.int)
confint(m.24h.int,level=0.95)

###############24h,basal##########

data = read.csv("dendrite_spd_24h.csv")
data = as.data.frame(data[data$apical!="apical",])
str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)

#select model

m.24h.int  <- lmer(spine_density ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.add  <- lmer(spine_density ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.sham <- lmer(spine_density ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.24h.ipsi <- lmer(spine_density ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.null  = lmer(spine_density ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.24h.int,m.24h.add,m.24h.sham,m.24h.ipsi,m.24h.null)# again: the interaction should be kept

plot(m.24h.int)
summary(m.24h.int)
confint(m.24h.int,level=0.95) #significant at 95%


library(lme4)

###########48h, overall##########
data = read.csv("dendrite_spd_48h.csv")

str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)

#select model

m.48h.int  <- lmer(spine_density ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.add  <- lmer(spine_density ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.sham <- lmer(spine_density ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.48h.ipsi <- lmer(spine_density ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.null  = lmer(spine_density ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.48h.int,m.48h.add,m.48h.sham,m.48h.ipsi,m.48h.null)# again: the interaction should be kept

plot(m.48h.sham)
summary(m.48h.sham)
confint(m.48h.sham,level=0.95)

###############48h,apical##########

data = read.csv("dendrite_spd_48h.csv")
data = as.data.frame(data[data$apical=="apical",])
str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)

#select model

m.48h.int  <- lmer(spine_density ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.add  <- lmer(spine_density ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.sham <- lmer(spine_density ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.48h.ipsi <- lmer(spine_density ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.null  = lmer(spine_density ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.48h.int,m.48h.add,m.48h.sham,m.48h.ipsi,m.48h.null)# again: the interaction should be kept

plot(m.48h.int)
summary(m.48h.int)
confint(m.48h.int,level=0.95)

###############48h,basal##########

data = read.csv("dendrite_spd_48h.csv")
data = as.data.frame(data[data$apical!="apical",])
str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)

#select model

m.48h.int  <- lmer(spine_density ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.add  <- lmer(spine_density ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.sham <- lmer(spine_density ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.48h.ipsi <- lmer(spine_density ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.null  = lmer(spine_density ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.48h.int,m.48h.add,m.48h.sham,m.48h.ipsi,m.48h.null)# again: the interaction should be kept

plot(m.48h.sham)
summary(m.48h.sham)
confint(m.48h.sham,level=0.95)


