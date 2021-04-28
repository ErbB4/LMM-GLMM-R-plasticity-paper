#analyses for c-fos protein expression

library(lme4)
# importin data
data = read.csv("c-fos.csv")

# this is just to check whether evrything is imported correctly
# You need to specifiy certain variables as factors. R will think that they are on a continous
# Or interval scale, since they are registered as numbers. However, things like "sham"
# are clearly your treatment categories and you need to tell this to the program if you dont use
# letters for coding. The same with Random "FACTORS"
str(data)
head(data)
data$Time <- as.factor(data$Time) #
data$Sham <- as.factor(data$Sham) # this needs to be a factpr since it specifies your treatment categories
data$Ipsi <- as.factor(data$Ipsi) #
data$Distance2 <- as.factor(data$Distance2)
data$Distance1 <- as.factor(data$Distance1)
data$AnimalNo <- as.factor(data$AnimalNo)# also here, the animal are not a continous scale but separate identities


# Since you have counts as your dependent variable, you have to use the glmer
# function. You must assume that the data comes from a poisson (not a normal) distribution
# It makes the modelling a bit more complicated, but it is the correct way to do it like this

# Referring to Figure 2: If you show a figure like this, I would assume
# that you want to model the counts in dependence on:
# 1st: the stimulation treatment, 2nd: the distance to optic fiber
# 3rd: the ipsi and contra side, 4th: the time since treatment
# and ALL OF THESE INTERACTIONS
# This would become a very complicated model and I always recommend to be very specific
# about your hypotheses in the first place, which may simplify your model structure. You would 
# also have to model separately for each time interval, since all units are nested within Time
# The models would look like this:

time1 <- as.data.frame(data[data$Time=="1",])
time2 <- as.data.frame(data[data$Time=="2",])
time3 <- as.data.frame(data[data$Time=="3",])

model_Time1 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo), data = data)
model_Time2 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo), data = data)
model_Time3 = glmer(Counting ~ Sham*Ipsi*Distance1 + (1|AnimalNo), data = data)

# Then I would recommend some model averaging procedure for each model, to
# select the best candidate model and afterwards look at the effect strengths of each fixed factor in the final model.
# You can use the dredge function of the MuMIn package for this and must fit
# each model with "ML" first. Let me know if that is something you may be 
# interested in, and I can write the script for you....


# However, based on your document, I understand that you are interested in the
# the effects of i) stimulation, ii) ipsi vs contra, and iii) the interaction of these two
# in this case, I recommend doing a model comparison based on information criteria
# you can e.g. use the AIC or AICc for this. You define the most complex and all simpler
# models based on your hypothesis and compare the AIC. The model with the lowest AIC
# would be the best candidate model to "explain" your data.
# Random effects: here you have to model separately for each time interval, since
# "Time" is not replicated at all. IN this case, you have to interpret each the results
# from each Time separately!
# For each Time, you can account for the fact that measures within individuals are not 
# independent and the fact that there is a bit of variation between individuals and distances within 
# individual Animals (as indicated by these figures and the one in your document). 

library(lattice)
trellis.par.set("background$col" = "white")

# For Time 1 There are some animals which stick out. Based on the scatter, the distance might not have a large impact
xyplot(Counting ~ Distance1 | AnimalNo, pch = 16, cex = 1.3,
       par.strip.text = list(cex = 1.5), scales = list(alternating = F), data=time1)
# For Time 2: very similar here
xyplot(Counting ~ Distance1 | AnimalNo, pch = 16, cex = 1.3,
       par.strip.text = list(cex = 1.5), scales = list(alternating = F), data=time2)
# For Time 3: here might be a slight distance effect for only animal 22...
xyplot(Counting ~ Distance1 | AnimalNo, pch = 16, cex = 1.3,
       par.strip.text = list(cex = 1.5), scales = list(alternating = F), data=time3)

# defining the random structure for each model as (1|AnimalNo/Distance1) also acounts 
# somewhat for the un balance you have with regard to the number of values in the different distance 
# categories

# IN general about glmer: The data is internally "transformed" and modelled on the so called
# "link scale". In your case its a log transformation. You can judge the statistical significance of each factor
# by looking at whether the confidence intervals overlap zero or not, using the values on link scale.
# if you want to plot the predicted counts from your model, you have to back-transform to "response scale".
# You can easily specify this with the "effects" package in R. I give you examples below...

# Time 1: it seems like the interaction model is the best way to describe your data!
m.T1.int <- glmer(Counting ~ Sham * Ipsi + (1|AnimalNo/Distance1), family=poisson, data = time1)
m.T1.add <- glmer(Counting ~ Sham + Ipsi + (1|AnimalNo/Distance1), family=poisson, data = time1)
m.T1.sham <- glmer(Counting ~ Sham + (1|AnimalNo/Distance2), family=poisson, data = time1)
m.T1.ipsi <- glmer(Counting ~ Ipsi + (1|AnimalNo/Distance1), family=poisson, data = time1)
m.T1.null  = glmer(Counting ~ 1 + (1|AnimalNo/Distance1), family=poisson, data = time1)
AIC(m.T1.int,m.T1.add,m.T1.sham,m.T1.ipsi,m.T1.null)#

# Then check whether the model fulfills the assumptions: 
# residuals look very good
plot(m.T1.int)
qqnorm(residuals(m.T1.int))
qqline(residuals(m.T1.int))
# in glms, you also have to check for over-dispersion
# Dispersion is a factor by which the variance is 
# higher/lower than assumed by the model. A rough estimation of checking this is by dividing
# the residual deviance of the model by the degrees of freedom. The value should be between
# 0.6 and 2 (around 1). This was absolutely not fulfilled here and your model was highly
# ober-dispersed. You can also test this
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(m.T1.int)
# The issue causes some difficulties, but the easiest way to deal with that is to
# assume a negative binomial distribution for your data, which assumes that the dispersion parameter
# can deviate from one.
m.T1.int.b <- glmer.nb(Counting ~ Sham * Ipsi +  (1|AnimalNo/Distance1),  data = time1) # for some reason, this model is over-fitted too. Maybe because the distance has to few levels to be properly estimated. 
# I therefore included it as fixed effect, but then got other porblems. So I excluded the AnimalNo and kept Distance to somehow account for the unbalance here
m.T1.int.b <- glmer.nb(Counting ~ Sham * Ipsi +  (1|Distance2),  data = time1)
plot(m.T1.int.b) # now the assumptions look good
qqnorm(residuals(m.T1.int.b))
qqline(residuals(m.T1.int.b))
# then you can extract the effect strengths on link scale (log): you can use these to judge stat significance
summary(m.T1.int.b) #
confint(m.T1.int.b) # these are the 95% CIs. It corresponds to the p values in the summary () output
# You can get the estimated counts (response scale) from your model using the effects package
library(effects)
eff.T1 <- allEffects(m.T1.int.b)# These are the effects on response scale, meaning the counts
plot(eff.T1)# you can also extract the effects and their confidence intervals. Just check what the package has to offer


# Time 2: same here, interaction term is important
m.T2.int <- glmer(Counting ~ Sham * Ipsi + (1|AnimalNo/Distance1), family=poisson, data = time2)
m.T2.add <- glmer(Counting ~ Sham + Ipsi + (1|AnimalNo/Distance1), family=poisson, data = time2)
m.T2.sham <- glmer(Counting ~ Sham + (1|AnimalNo/Distance1), family=poisson, data = time2)
m.T2.ipsi <- glmer(Counting ~ Ipsi + (1|AnimalNo/Distance1), family=poisson, data = time2)
m.T2.null  = glmer(Counting ~ 1 + (1|AnimalNo/Distance1), family=poisson, data = time2)
AIC(m.T2.int,m.T2.add,m.T2.sham,m.T2.ipsi,m.T2.null)# again: the interaction should be kept
overdisp_fun(m.T2.int) # we fit the negative binomial version again
m.T2.int.b <- glmer.nb(Counting ~ Sham * Ipsi +  (1|AnimalNo/Distance1),  data = time2) # same issue as for T1
m.T2.int.b <- glmer.nb(Counting ~ Distance1 +Sham * Ipsi +  (1|AnimalNo),  data = time2) # this solution does not work in this case. I dropped the random effects and used the distance as fixed co-variate
library(MASS)
m.T2.int.b <- glm.nb(Counting ~ Distance1 +Sham * Ipsi,  data = time2) #
plot(m.T2.int.b) # we should remove point 105 as an outlier
time2.b <- as.data.frame(time2[-105,])
m.T2.int.b <- glm.nb(Counting ~ Distance1 +Sham * Ipsi,  data = time2.b) #
plot(m.T2.int.b) # this looks ok
summary(m.T2.int.b) # stimulationa and the interaction with ipsi are significant. Please ignore the effects of distance in this case
confint(m.T2.int.b)
eff.T2 <- allEffects(m.T2.int)#
plot(eff.T2)

# Time 3: here the interaction is significant too!
m.T3.int <- glmer(Counting ~ Sham * Ipsi + (1|AnimalNo/Distance1), family=poisson, data = time3)
m.T3.add <- glmer(Counting ~ Sham + Ipsi + (1|AnimalNo/Distance1), family=poisson, data = time3)
m.T3.sham <- glmer(Counting ~ Sham + (1|AnimalNo/Distance1), family=poisson, data = time3)
m.T3.ipsi <- glmer(Counting ~ Ipsi + (1|AnimalNo/Distance1), family=poisson, data = time3)
m.T3.null  = glmer(Counting ~ 1 + (1|AnimalNo/Distance1), family=poisson, data = time3)
AIC(m.T3.int,m.T3.add,m.T3.sham,m.T3.ipsi,m.T3.null)#
overdisp_fun(m.T3.int) # also here, we assume a negative binomial dist
m.T3.int.b <- glmer.nb(Counting ~ Sham * Ipsi +  (1|AnimalNo/Distance1),  data = time3) # we have the same problems as for model Time2
m.T3.int.b <- glm.nb(Counting ~ Distance1 +Sham * Ipsi,  data = time3) #
plot(m.T3.int.b) # I removed point 338
data2 <- as.data.frame(data[-338,])
time3.b <- as.data.frame(data2[data2$Time=="3",])
m.T3.int.b <- glm.nb(Counting ~ Distance1 +Sham * Ipsi,  data = time3.b) #
plot(m.T3.int.b) # looks good now
summary(m.T3.int.b) # 
confint(m.T3.int.b)
eff.T3 <- allEffects(m.T3.int)#
plot(eff.T3)

