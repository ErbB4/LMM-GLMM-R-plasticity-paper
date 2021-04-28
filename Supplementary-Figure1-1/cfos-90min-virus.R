library(lme4)
library(MuMIn)

#define the function for overdispersion test
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

#load data
data = read.csv("c-fos.csv")
options(na.action = "na.fail")

#set data as nominal type
data$Time <- as.factor(data$Time)
data$Sham <- as.factor(data$Sham)
data$Ipsi <- as.factor(data$Ipsi) 
data$Distance2 <- as.factor(data$Distance2) #distance2 is nominal data 1,2,3,4,5
#distance1 is re-formed value 0,1,2, labeing the relative distance from small to large
#so distance1 is not redefined as nominal type, data$Distance1 <- as.factor(data$Distance1) 
data$AnimalNo <- as.factor(data$AnimalNo)

#split data for 90min, 24hrs, and 48hrs
time1 <- as.data.frame(data[data$Time=="1",])
time2 <- as.data.frame(data[data$Time=="2",])
time3 <- as.data.frame(data[data$Time=="3",])


#Time1, 90min
#candidate models of the selection for optimal random effects structure
#data type: cell counting

mod1 = glm(Counting ~ Sham*Ipsi*Distance1, family=poisson,data = time1)
mod2 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo/Distance1), family=poisson,data = time1)
mod3 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1+Distance1|AnimalNo), family=poisson,data = time1)
mod4 = glmer(Counting ~ Sham*Ipsi + (1|AnimalNo) + (1|Distance1), family=poisson,data = time1)
mod5 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo), family=poisson,data = time1)

#rank all the candidate models by AIC values
AIC(mod1,mod2,mod5) #mod3 and mod4 is problematic and excluded

#mod2 is selected with the optimal random effects structure
#so mod2 is used as the global model for dredge selection
dd2 <- dredge(mod2)
b2 <- get.models(dd2, 6)[[1]] #get the best-fit model #test for the top 6 models
overdisp_fun(b2) #test the overdispersion of the best-fit model

#the b2 model showed overdispersion (with value >2)
#re-tunning of the model by assuming a negative binomial distribution
mod1 = glm.nb(Counting ~ Sham*Ipsi*Distance1,data = time1)
mod2 = glmer.nb(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo/Distance1),data = time1)
mod3 = glmer.nb(Counting ~ Sham*Ipsi*Distance1 + (1+Distance1|AnimalNo), data = time1)
mod4 = glmer.nb(Counting ~ Sham*Ipsi + (1|AnimalNo) + (1|Distance1), data = time1)
mod5 = glmer.nb(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo), data = time1)

#mod1 is the only one without singular problem

dd2 <- dredge(mod1)
b2 <- get.models(dd2, 1)[[1]]


overdisp_fun(b2) #great!
plot(mod2)#looks good
summary(mod2) #sham is significant ***, interaction between sham and ipsi is significant **



