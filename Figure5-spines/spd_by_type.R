library(lme4)


###############24h,apical##########

data = read.csv("spine_density_by_type_24h.csv")
data = as.data.frame(data[data$apical=="apical",])
str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)
data$type <-    as.factor(data$Surpass.Object)

data = as.data.frame(data[data$Surpass.Object=="Filopodia / Dendrite",]) #"Filopodia / Dendrite","Long Thin", "Stubby", "Mushroom"
#select model

m.24h.int  <- lmer(Dendrite_Spine_Density ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.add  <- lmer(Dendrite_Spine_Density ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.sham <- lmer(Dendrite_Spine_Density ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.24h.ipsi <- lmer(Dendrite_Spine_Density ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.null  = lmer(Dendrite_Spine_Density ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.24h.int,m.24h.add,m.24h.sham,m.24h.ipsi,m.24h.null)# again: the interaction should be kept

plot(m.24h.add)
summary(m.24h.add)
confint(m.24h.add,level=0.95) 

#Filopodia is significant at 99%
#Long Thin is not significant
#Stubby is significant at 95%
#mushroom is not significant

###############24h,basal##########

data = read.csv("spine_density_by_type_24h.csv")
data = as.data.frame(data[data$apical!="apical",])
str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)

data$type <-    as.factor(data$Surpass.Object)

data = as.data.frame(data[data$Surpass.Object=="Stubby",]) #"Filopodia / Dendrite","Long Thin", "Stubby", "Mushroom"

#select model

m.24h.int  <- lmer(Dendrite_Spine_Density ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.add  <- lmer(Dendrite_Spine_Density ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.sham <- lmer(Dendrite_Spine_Density ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.24h.ipsi <- lmer(Dendrite_Spine_Density ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.null  = lmer(Dendrite_Spine_Density ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.24h.int,m.24h.add,m.24h.sham,m.24h.ipsi,m.24h.null)# again: the interaction should be kept

plot(m.24h.sham)
summary(m.24h.sham)
confint(m.24h.sham,level=0.95) 

#Filopodia is significant at 95%
#long thin is not significant
#stubby is significant at 95%
#mushroom is not significant


###############48h,apical##########

data = read.csv("spine_density_by_type_48h.csv")
data = as.data.frame(data[data$apical=="apical",])
str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)
data$type <-    as.factor(data$Surpass.Object)

data = as.data.frame(data[data$Surpass.Object=="Mushroom",]) #"Filopodia / Dendrite","Long Thin", "Stubby", "Mushroom"

#select model

m.48h.int  <- lmer(Dendrite_Spine_Density ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.add  <- lmer(Dendrite_Spine_Density ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.sham <- lmer(Dendrite_Spine_Density ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.48h.ipsi <- lmer(Dendrite_Spine_Density ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.null  = lmer(Dendrite_Spine_Density ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.48h.int,m.48h.add,m.48h.sham,m.48h.ipsi,m.48h.null)# again: the interaction should be kept

plot(m.48h.int)
summary(m.48h.int)
confint(m.48h.sham,level=0.95)


#long thin is significant at 95%
#the others are not significany

###############48h,basal##########

data = read.csv("spine_density_by_type_48h.csv")
data = as.data.frame(data[data$apical!="apical",])
str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot)
data$Ipsi <- as.factor(data$IpsiOrNot)
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)
data$type <-    as.factor(data$Surpass.Object)

data = as.data.frame(data[data$Surpass.Object=="Mushroom",]) #"Filopodia / Dendrite","Long Thin", "Stubby", "Mushroom"

#select model

m.48h.int  <- lmer(Dendrite_Spine_Density ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.add  <- lmer(Dendrite_Spine_Density ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.sham <- lmer(Dendrite_Spine_Density ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.48h.ipsi <- lmer(Dendrite_Spine_Density ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.null  = lmer(Dendrite_Spine_Density ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.48h.int,m.48h.add,m.48h.sham,m.48h.ipsi,m.48h.null)# again: the interaction should be kept

plot(m.48h.sham)
summary(m.48h.sham)
confint(m.48h.sham,level=0.95)

#stubby and mushroom is significant at 95%
#the other two are not significant
