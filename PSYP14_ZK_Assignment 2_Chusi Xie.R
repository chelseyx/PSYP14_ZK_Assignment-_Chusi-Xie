##1  Loading packages 
library(dplyr)    # for data manipulation
library(tidyverse)# for dplyr and ggplot2,make data wrangling easier
library(ggplot2)  # for data visualization 
library(psych)    # for describe
library(MVN)      # 
library(car)
require(lsr)

require(sciplot)
library(gridExtra)
library(broom)
library(gpairs)
library(lmtest)
library(lm.beta)  # for lm.beta  (standardized regression coefficients)


##2 Custom functions
coef_table = function(model) {
  require(lm.beta)
  mod_sum =summary(model)
  mod_sum_p_values =as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" 
                   & mod_sum_p_values !="1"] =substr(mod_sum_p_values[mod_sum_p_values != "0" 
                                                                      & mod_sum_p_values !="1"], 2,nchar(mod_sum_p_values[mod_sum_p_values !="0" 
                                                                                                                          & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  mod_sum_table =cbind(as.data.frame(round(cbind(coef(model),confint(model),
                                                 c(0,lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),2)), 
                       mod_sum_p_values)
  
  names(mod_sum_table) =c("b", "95%CI lb", "95%CI ub", "Std.Beta","p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  
  return(mod_sum_table)
}

##3  Load data 
mydata<-read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

mydata2<-read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")



##4  Check the dataset for irregularities
describe(mydata)
str(mydata)
table(mydata$sex)

# remove the data which is seem to be impossible
mydata<-mydata%>%
  filter(age<=150)%>% ##Until now no one can lives longer than 150.
  filter(household_income>=0)%>%  ##The household income need to more than 0.
  filter(STAI_trait>=20)##The State Trait Anxiety Inventory - T: measures trait anxiety on a scale of 20 to 80

#convert the houesehold income
mydata <- mydata %>% mutate(household_income_thousand = household_income/1000 )
                                                          
##5 Regression model
#5.1 model 1


backward_model_origin<- lm(pain~age+ sex+STAI_trait+ pain_cat+mindfulness+cortisol_serum+ weight+ IQ+ household_income_thousand,data=mydata)
backward_model_step<- step(backward_model_origin, direction="backward", trace = F)

backward_model<-lm(pain~age+sex+pain_cat+mindfulness+cortisol_serum + household_income_thousand,data=mydata)


###6 assumptions test of linear regression
  #6.1 QQ plot
  qqPlot(backward_model)
  
  
  # histogram
  residuals_backward_model =enframe(residuals(backward_model))
  residuals_backward_model %>%
    ggplot() +aes(x = value) +
    geom_histogram()
  
  # skew and kurtosis
  describe(residuals(backward_model))
  ##result show that the model meet the assumption of normality
  
  
  
  ##6.2  Linearity:the relationship between the outcome variable and the predictor(s) must be linear.
  backward_model %>%residualPlots()
  
  ##In our case, even though there is minor curvature visible on the mindfulness and household_income, 
  ##the tests are all non significant, so the linearity assumption seems to hold true for our model.
  
  
  ##6.3  Homoscedasticty
  backward_model %>%plot(which = 3)
  backward_model %>%ncvTest()# NCV test
  backward_model %>%bptest()
  #These test indicate that there is not significant heteroscedasticity in backward_model 
  
  
  ##6.4  No multicollinearity
  backward_model %>%vif()
  ##Because no VIF values are more than 3, multicollinearity is not  problematic.



#7  Comparing two modelS and testing performance on the new dataset
  
  backward_model_origin<- lm(pain~age+ sex+STAI_trait+ pain_cat+mindfulness+cortisol_serum+ weight+ IQ+ household_income_thousand,data=mydata)
  summary(backward_model_origin)
  AIC(backward_model_origin)
  coef_table(backward_model_origin)
  
  
  backward_model<-lm(pain~age+sex+pain_cat+mindfulness+cortisol_serum + household_income_thousand,data=mydata)
  summary(backward_model)
  AIC(backward_model)
  confint(backward_model)
  lm.beta(backward_model) 
  coef_table(backward_model)
  
  anova(backward_model,backward_model_origin)
  
  
  theory_based_model<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_saliva,data=mydata)
  summary(theory_based_model)
  AIC(theory_based_model)

  #look at the adj. R squared statistic to see how much variance is explained by the new and the oldmodel.
  
  summary(backward_model)$adj.r.squared
  summary(theory_based_model)$adj.r.squared
  
  
  AIC(backward_model)
  AIC(theory_based_model)


###8 Testing performance on the mydata2
  #before predict the mydata2, convert the household data
  mydata2 <- mydata2 %>% mutate(household_income_thousand = household_income/1000 )

  #predict mydata1
  prediction_backwardM1<-predict(backward_model, mydata)
  prediction_theoryM1<-predict(theory_based_model,mydata)
  
  RSS_prediction_backwardM1<-sum((mydata[, "pain"] - prediction_backwardM1)^2)
  RSS_prediction_theoryM1<-sum((mydata[, "pain"] - prediction_theoryM1)^2)
  
  RSS_prediction_backwardM1
  RSS_prediction_theoryM1
  
  #predict mydata2
  prediction_backwardM2<-predict(backward_model, mydata2)
  prediction_theoryM2<-predict(theory_based_model,mydata2)
  
  RSS_prediction_backwardM2<-sum((mydata2[, "pain"] - prediction_backwardM2)^2)
  RSS_prediction_theoryM2<-sum((mydata2[, "pain"] - prediction_theoryM2)^2)
  
  RSS_prediction_backwardM2
  RSS_prediction_theoryM2
  
  #This test reveals that the backward regression model has more error 
  #than the theory based model.






