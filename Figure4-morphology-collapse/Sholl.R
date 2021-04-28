library(lme4)
library(MuMIn)

options(na.action = "na.fail")

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

#############24h##############
data = read.csv("Sholl_24h.csv")
data <- as.data.frame(data_raw[data_raw$Radius>60 & data_raw$Radius<200,]) #tested two subdatasets: 0-60 and 60+ 

str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot) 
data$Ipsi <- as.factor(data$IpsiOrNot) 
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)

#data type is counting
mod3 = glmer(Sholl_Intersections ~ Sham*Ipsi+ (1|Radius) + (1|AnimalNo),family=poisson,data=data)
mod4 = glmer(Sholl_Intersections ~ Sham*Ipsi+ (1|Radius/Ipsi) + (1|AnimalNo),family=poisson,data=data)
mod5 = glmer(Sholl_Intersections ~ Sham*Ipsi+ (1|Radius) + (1|Ipsi) + (1|AnimalNo),family=poisson,data=data)


AIC(mod3,mod4,mod5)# mod3 is better
dd2 <- dredge(mod3)
b1 <- get.models(dd2, 1)[[1]]

overdisp_fun(b1) #test overdispersion
summary(b1)

#in both segments, the sham and stimulate were not significant different


#############48h##############
data = read.csv("Sholl_48h.csv")
data <- as.data.frame(data_raw[data_raw$Radius>40 & data_raw$Radius<150,]) #tested two subdatasets: 0-40 and 40+ 

str(data)
head(data)
data$Sham <- as.factor(data$ShamOrNot) 
data$Ipsi <- as.factor(data$IpsiOrNot) 
data$AnimalNo <- as.factor(data$AnimalID)
data$NeuronNo <- as.factor(data$NeuronID)

#data type is counting
mod3 = glmer(Sholl_Intersections ~ Sham*Ipsi+ (1|Radius) + (1|AnimalNo),family=poisson,data=data)
mod4 = glmer(Sholl_Intersections ~ Sham*Ipsi+ (1|Radius/Ipsi) + (1|AnimalNo),family=poisson,data=data)
mod5 = glmer(Sholl_Intersections ~ Sham*Ipsi+ (1|Radius) + (1|Ipsi) + (1|AnimalNo),family=poisson,data=data)


AIC(mod3,mod4,mod5)# mod3 is better
dd2 <- dredge(mod3)
b1 <- get.models(dd2, 1)[[1]]
overdisp_fun(b1) 
summary(b1)

#sham and stimulated were not significantly different