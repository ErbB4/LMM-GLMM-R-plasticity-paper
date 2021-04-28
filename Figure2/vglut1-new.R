#analyses for VGLUT1 protein expression

library(lme4)
# importin data
data = read.csv("vglut1.csv")

# specifiy certain variables as factors
str(data)
head(data)
data$Time <- as.factor(data$Time)
data$Sham <- as.factor(data$Sham)
data$Ipsi <- as.factor(data$Ipsi)
data$Distance2 <- as.factor(data$Distance2)
data$Distance1 <- as.factor(data$Distance1)
data$AnimalNo <- as.factor(data$AnimalNo)

#get data for each time point

time1 <- as.data.frame(data[data$Time=="1",])
time2 <- as.data.frame(data[data$Time=="2",])
time3 <- as.data.frame(data[data$Time=="3",])

time1$Intensity <- time1$Intensity/3998422.916666665
time2$Intensity <- time2$Intensity/3985192.5
time3$Intensity <- time3$Intensity/3617928.0


# Time 1
m.T1.int <- lmer(Intensity ~ Sham * Ipsi + (1|AnimalNo/Distance2),data = time1)
m.T1.add <- lmer(Intensity ~ Sham + Ipsi + (1|AnimalNo/Distance2),data = time1)
m.T1.sham <- lmer(Intensity ~ Sham + (1|AnimalNo/Distance2),data = time1)
m.T1.ipsi <- lmer(Intensity ~ Ipsi + (1|AnimalNo/Distance2),data = time1)
m.T1.null  = lmer(Intensity ~ 1 + (1|AnimalNo/Distance2),data = time1)
AIC(m.T1.int,m.T1.add,m.T1.sham,m.T1.ipsi,m.T1.null)# interaction model is the best

# Then check whether the model fulfills the assumptions: 
# residuals look very good
library(lattice)
trellis.par.set("background$col" = "white")
par(mar = rep(2, 4))
plot(m.T1.int)
qqnorm(residuals(m.T1.int))
qqline(residuals(m.T1.int))

#print confidence interval
summary(m.T1.int) 
confint(m.T1.int) # not significant


# Time 2
m.T2.int <- lmer(Intensity ~ Sham * Ipsi + (1|AnimalNo/Distance2),data = time2)
m.T2.add <- lmer(Intensity ~ Sham + Ipsi + (1|AnimalNo/Distance2),data = time2)
m.T2.sham <- lmer(Intensity ~ Sham + (1|AnimalNo/Distance2),data = time2)
m.T2.ipsi <- lmer(Intensity ~ Ipsi + (1|AnimalNo/Distance2),data = time2)
m.T2.null  = lmer(Intensity ~ 1 + (1|AnimalNo/Distance2),data = time2)
AIC(m.T2.int,m.T2.add,m.T2.sham,m.T2.ipsi,m.T2.null)# again: the interaction should be kept

plot(m.T2.int) # this looks ok
summary(m.T2.int) # stimulationa and the interaction with ipsi are significant. Please ignore the effects of distance in this case
confint(m.T2.int,level=0.995)

# Time 3: here the interaction is significant too!
m.T3.int <- lmer(Intensity ~ Sham * Ipsi + (1|AnimalNo/Distance2),data = time3)
m.T3.add <- lmer(Intensity ~ Sham + Ipsi + (1|AnimalNo/Distance2),data = time3)
m.T3.sham <- lmer(Intensity ~ Sham + (1|AnimalNo/Distance2),data = time3)
m.T3.ipsi <- lmer(Intensity ~ Ipsi + (1|AnimalNo/Distance2),data = time3)
m.T3.null  = lmer(Intensity ~ 1 + (1|AnimalNo/Distance2),data = time3)
AIC(m.T3.int,m.T3.add,m.T3.sham,m.T3.ipsi,m.T3.null)#
plot(m.T3.int) # I removed point 338
summary(m.T3.int) # 
confint(m.T3.int,level=0.95)

