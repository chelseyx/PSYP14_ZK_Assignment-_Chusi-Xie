##1  Loading packages

library(plyr)     #revaluate data
library(dplyr)
library(tidyverse)# for tidy code and ggplot 
library(car)      # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest)   # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot)     # for bootstrapping
library(lmboot)   # for wild bootsrapping
library(psych)    # for describe
library(cAIC4)    # for cAIC
library(r2glmm)   # for r2beta
library(lme4)     # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn)    # for r.squaredGLMM
library(optimx)   # for optimx optimizer
library(ggExtra)  #for plot, function:ggMarginal
library(MuMIn) #marginal and conditional R squared values


require(lsr)
require(sciplot)
library(ggplot2)
library(gridExtra)
library(broom)
library(gpairs)

##2 Custom functions

stdCoef.merMod <-function(object) {
  sdy <-sd(getME(object,"y"))
  sdx <-apply(getME(object,"X"), 2, sd)
  sc <-fixef(object)*sdx/sdy
  se.fixef <-coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

##3  Load data 
mydata3<-read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")
mydata4<-read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")


#4  Check the dataset
str(mydata3)
str(mydata4)
summary(mydata3)
describe(mydata3)

mydata3$ID<-as.factor(mydata3$ID)
mydata3$sex<-as.factor(mydata3$sex)
mydata3$hospital<-as.factor(mydata3$hospital)

mydata4$ID<-as.factor(mydata4$ID)
mydata4$sex<-as.factor(mydata4$sex)
mydata4$hospital<-as.factor(mydata4$hospital)
#change miscoding data
mydata3$sex <- revalue(mydata3$sex , c("femlae"="female"))

#convert the houesehold income
mydata3 <- mydata3 %>% mutate(household_income_thousand = household_income/1000 )
mydata4 <- mydata4 %>% mutate(household_income_thousand = household_income/1000 )


# remove the data which is seem to be impossible
mydata3<-mydata3%>%
  filter(household_income>0)

mydata4<-mydata4%>%
  filter(household_income>0)

#check the data again
describe(mydata3)
describe(mydata4)


#visualization
plot_age<- ggplot(mydata3, aes(age)) + 
  geom_histogram(aes(y=..density..), colour = "blue", fill = "white") + 
  geom_density( alpha =0.2, fill = "#FF6666")+
  geom_vline( aes(xintercept=mean(STAI_trait), color="red"), linetype ="dashed", size = 1)

plot1<- ggplot(mydata3, aes(STAI_trait)) + 
  geom_histogram(aes(y=..density..), colour = "blue", fill = "white") + 
  geom_density( alpha =0.2, fill = "#FF6666")+
  geom_vline( aes(xintercept=mean(STAI_trait), color="red"), linetype ="dashed", size = 1)

plot2<-ggplot(mydata3, aes(pain_cat)) + 
  geom_histogram(aes(y=..density..),  colour = "blue", fill = "white") + 
  geom_density( alpha =0.2, fill = "#FF6666")+
  geom_vline( aes(xintercept=mean(pain_cat), color="red"), linetype ="dashed", size = 1)

plot3<-ggplot(mydata3, aes(cortisol_saliva)) + 
  geom_histogram(aes(y=..density..), colour = "blue", fill = "white") + 
  geom_density( alpha =0.2, fill = "#FF6666")+
  geom_vline( aes(xintercept=mean(cortisol_saliva), color="red"), linetype ="dashed", size = 1)

plot4<-ggplot(mydata3, aes(mindfulness)) + 
  geom_histogram(aes(y=..density..),  colour = "blue", fill = "white") + 
  geom_density( alpha =0.2, fill = "#FF6666")+
  geom_vline( aes(xintercept=mean(mindfulness), color="red"), linetype ="dashed", size = 1)

grid.arrange(plot1,plot2,plot3,plot4, ncol=2)

#exploring hospital predictors
mydata3 %>%
  ggplot() +
  aes(x = hospital , y = pain) +
  geom_boxplot()


##5  Mixed models
  
#5.1
  #fixed model:
  mod_fixed <-lm(pain~age+sex+STAI_trait+pain_cat+cortisol_saliva+mindfulness, data = mydata3)
  summary(mod_fixed)
 
  
  #random intercept model:
  mod_intercept <-lmer(pain~age+sex+STAI_trait+pain_cat+cortisol_saliva+mindfulness+(1|hospital), data = mydata3)
  summary(mod_intercept)
  
  # marginal R squared with confidence intervals
  r2beta(mod_intercept, method = "nsj", data = mydata3)
  
  ## marginal and conditional R squared values
  r.squaredGLMM(mod_intercept)
  
  ##Cofidence intervals for the model coefficients:
  confint(mod_intercept)
  
  #Standardized beta for each predictor
  stdCoef.merMod(mod_intercept)
  

#5.2 predict dataset4 
  
  mydata4$prediction<-predict(mod_intercept , mydata4, allow.new.levels=TRUE)
  
  RSS = sum((mydata4$pain - mydata4$prediction)^2)
  TSS = sum((mydata4$pain - mean(mydata4$prediction))^2)
  R2 = 1-(RSS/TSS)
  R2
  
  m1=tapply(mydata3$pain, mydata3$hospital, mean)
  m2=tapply(mydata4$pain, mydata4$hospital, mean)
  summary(m1)
  summary(m2)
  
  t.test(m1,m2)
  
  
#5.3 bulding new random effect model with the most influencial predictor
  mod_slope <-lmer(pain ~ cortisol_saliva + (cortisol_saliva|hospital),control = lmerControl(optimizer ="Nelder_Mead"), data = mydata3)
  summary(mod_slope)
  mydata3$prediction3<-predict(mod_slope , mydata3)
  
  mod_intercept2 <-lmer(pain ~ cortisol_saliva + (1|hospital),control = lmerControl(optimizer ="Nelder_Mead"), data = mydata3)
  summary(mod_slope)
  
  #visualization
  mydata3 %>%
    ggplot() +
    aes(y = pain, x = cortisol_saliva, group = hospital)+
    geom_point(aes(color = hospital), size = 4) +
    geom_line(color='red', aes(y=prediction3, x=cortisol_saliva))+
    facet_wrap( ~ hospital, ncol = 2)
  
  # model fit indices
  AIC(mod_fixed)
  cAIC(mod_intercept)
  cAIC(mod_slope)
  cAIC(mod_intercept2)
  #anova
  anova(mod_intercept2,mod_slope)
 
  # marginal R squared with confidence intervals
  r2beta(mod_intercept, method = "nsj", data = mydata3)
  r2beta(mod_slope, method = "nsj", data = mydata3)
  r2beta(mod_intercept2, method = "nsj", data = mydata3)

   ## marginal and conditional R squared values
  r.squaredGLMM(mod_intercept)
  r.squaredGLMM(mod_slope)
  r.squaredGLMM(mod_intercept2)

  