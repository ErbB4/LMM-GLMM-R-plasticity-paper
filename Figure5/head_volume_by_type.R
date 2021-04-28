library(lme4)


###############24h,apical##########

data = read.csv("spine_head_volume_24h.csv")
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

m.24h.int  <- lmer(Spine_Part_Volume_Head ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.add  <- lmer(Spine_Part_Volume_Head ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.sham <- lmer(Spine_Part_Volume_Head ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.24h.ipsi <- lmer(Spine_Part_Volume_Head ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.null  = lmer(Spine_Part_Volume_Head ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.24h.int,m.24h.add,m.24h.sham,m.24h.ipsi,m.24h.null)

plot(m.24h.sham)
summary(m.24h.sham)
confint(m.24h.sham,level=0.95) 

#Filopodia is not significant
#Long Thin is significant at 95%
#Stubby is not significant
#mushroom is not significant

###############24h,basal##########

data = read.csv("spine_head_volume_24h.csv")
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

m.24h.int  <- lmer(Spine_Part_Volume_Head ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.add  <- lmer(Spine_Part_Volume_Head ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.sham <- lmer(Spine_Part_Volume_Head ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.24h.ipsi <- lmer(Spine_Part_Volume_Head ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.24h.null  = lmer(Spine_Part_Volume_Head ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.24h.int,m.24h.add,m.24h.sham,m.24h.ipsi,m.24h.null)# again: the interaction should be kept

plot(m.24h.sham)
summary(m.24h.sham)
confint(m.24h.sham,level=0.95) 

#Filopodia is not significant
#long thin is not significant
#stubby is not significant
#mushroom is not significant


###############48h,apical##########

data = read.csv("spine_head_volume_48h.csv")
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

m.48h.int  <- lmer(Spine_Part_Volume_Head ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.add  <- lmer(Spine_Part_Volume_Head ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.sham <- lmer(Spine_Part_Volume_Head ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.48h.ipsi <- lmer(Spine_Part_Volume_Head ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.null  = lmer(Spine_Part_Volume_Head ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.48h.int,m.48h.add,m.48h.sham,m.48h.ipsi,m.48h.null)

plot(m.48h.sham)
summary(m.48h.sham)
confint(m.48h.sham,level=0.95)


#stubby is not significant
#filopodia is not significant
#long thin is not significant
#mushroom not significany

###############48h,basal##########

data = read.csv("spine_head_volume_48h.csv")
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

m.48h.int  <- lmer(Spine_Part_Volume_Head ~ Sham * Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.add  <- lmer(Spine_Part_Volume_Head ~ Sham + Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.sham <- lmer(Spine_Part_Volume_Head ~ Sham  + (1|AnimalNo:NeuronNo) , data = data)
m.48h.ipsi <- lmer(Spine_Part_Volume_Head ~ Ipsi + (1|AnimalNo:NeuronNo) , data = data)
m.48h.null  = lmer(Spine_Part_Volume_Head ~ 1 + (1|AnimalNo:NeuronNo) , data = data)
AIC(m.48h.int,m.48h.add,m.48h.sham,m.48h.ipsi,m.48h.null)# again: the interaction should be kept

plot(m.48h.sham)
summary(m.48h.sham)
confint(m.48h.sham,level=0.95)

#mushroom is significant at 95%
#the other three are not significant
