library(lme4)
library(lattice)
trellis.par.set("background$col" = "white")

lv = 1 #change to 1,2,3,4,5

#############24h,apical##############
data = read.csv("dendrite_mean_diameter_24h.csv")
data <- as.data.frame(data[data$note=="include",])
data <- as.data.frame(data[data$apical=="apical",])
data <- as.data.frame(data[data$Level==lv,])

str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)
data$Level <- as.factor(data$Level)

#select model

m.24h.int <- lm(Dendrite_Mean_Diameter ~ Sham * Ipsi , data = data)
m.24h.add <- lm(Dendrite_Mean_Diameter ~ Sham + Ipsi , data = data)
m.24h.sham <- lm(Dendrite_Mean_Diameter ~ Sham , data = data)
m.24h.ipsi <- lm(Dendrite_Mean_Diameter ~ Ipsi , data = data)
m.24h.null  = lm(Dendrite_Mean_Diameter ~ 1 , data = data)
AIC(m.24h.int,m.24h.add,m.24h.sham,m.24h.ipsi,m.24h.null)# again: the interaction should be kept

plot(m.24h.sham)
summary(m.24h.sham)
confint(m.24h.sham,level=0.95)


#############24h,basal##############

data = read.csv("Dendrite_Mean_Diameter_24h.csv")
data <- as.data.frame(data[data$note=="include",])
data <- as.data.frame(data[data$apical!="apical",])

str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)
data$Level <- as.factor(data$Level)

#select model
m.24h.int <- lmer(Dendrite_Mean_Diameter ~ Sham * Ipsi + (1|Level), data = data)
m.24h.add <- lmer(Dendrite_Mean_Diameter ~ Sham + Ipsi + (1|Level)+(1|AnimalNo:NeuronNo), data = data)
m.24h.sham <- lmer(Dendrite_Mean_Diameter ~ Sham + (1|Level)+(1|AnimalNo:NeuronNo), data = data)
m.24h.ipsi <- lmer(Dendrite_Mean_Diameter ~ Ipsi + (1|Level)+(1|AnimalNo:NeuronNo), data = data)
m.24h.null  = lmer(Dendrite_Mean_Diameter ~ 1 + (1|Level)+(1|AnimalNo:NeuronNo), data = data)
AIC(m.24h.int,m.24h.add,m.24h.sham,m.24h.ipsi,m.24h.null)# again: the interaction should be kept

plot(m.24h.add)
summary(m.24h.add)
confint(m.24h.add)





#############48h,apical##############
data = read.csv("Dendrite_Mean_Diameter_48h.csv")
data <- as.data.frame(data[data$note=="include",])

data <- as.data.frame(data[data$apical=="apical",])

str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)
data$Level <- as.factor(data$Level)

#select model

m.48h.int <- lmer(Dendrite_Mean_Diameter ~ Sham * Ipsi + (1|Level) + (1|AnimalNo:NeuronNo) , data = data)
m.48h.add <- lmer(Dendrite_Mean_Diameter ~ Sham + Ipsi+ (1|Level) + (1|AnimalNo:NeuronNo) , data = data)
m.48h.sham <- lmer(Dendrite_Mean_Diameter ~ Sham+ (1|Level) + (1|AnimalNo:NeuronNo) , data = data)
m.48h.ipsi <- lmer(Dendrite_Mean_Diameter ~ Ipsi+ (1|Level) + (1|AnimalNo:NeuronNo) , data = data)
m.48h.null  = lmer(Dendrite_Mean_Diameter ~ 1 + (1|Level) + (1|AnimalNo:NeuronNo), data = data)
AIC(m.48h.int,m.48h.add,m.48h.sham,m.48h.ipsi,m.48h.null)# again: the interaction should be kept

plot(m.48h.int)
summary(m.48h.int)
confint(m.48h.int,level=0.95)

#############48h,basal##############


data = read.csv("Dendrite_Mean_Diameter_48h.csv")
data <- as.data.frame(data[data$note=="include",])
data <- as.data.frame(data[data$apical!="apical",])

str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)
data$Level <- as.factor(data$Level)

#select model
m.48h.int <- lmer(Dendrite_Mean_Diameter ~ Sham * Ipsi + (1|Level)+(1|AnimalNo:NeuronNo), data = data)
m.48h.add <- lmer(Dendrite_Mean_Diameter ~ Sham + Ipsi + (1|Level)+(1|AnimalNo:NeuronNo), data = data)
m.48h.sham <- lmer(Dendrite_Mean_Diameter ~ Sham + (1|Level)+(1|AnimalNo:NeuronNo), data = data)
m.48h.ipsi <- lmer(Dendrite_Mean_Diameter ~ Ipsi + (1|Level)+(1|AnimalNo:NeuronNo), data = data)
m.48h.null  = lmer(Dendrite_Mean_Diameter ~ 1 + (1|Level)+(1|AnimalNo:NeuronNo), data = data)
AIC(m.48h.int,m.48h.add,m.48h.sham,m.48h.ipsi,m.48h.null)# again: the interaction should be kept

plot(m.48h.int)
summary(m.48h.int)
confint(m.48h.int,level=0.95)


