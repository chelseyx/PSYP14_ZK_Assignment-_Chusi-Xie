##1  Loading packages 
library(dplyr)    # for data manipulation
library(tidyverse)# for dplyr and ggplot2,make data wrangling easier
library(ggplot2)  # for data visualization 
library(psych)    # for describe
library(MVN)      
library(car)
require(lsr)

require(sciplot)
library(gridExtra)
library(broom)
library(gpairs)
library(lmtest)
library(lm.beta)  # for lm.beta  (standardized regression coefficients)

##2 Custom function

#2.1 For visualization the error of the predictions of the regressions line
error_plotter <- function(mod, col = "black", x_var = NULL){
  mod_vars = as.character(mod$call[2])
  data = as.data.frame(eval(parse(text = as.character(mod$call[3]))))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))
  data$pred = predict(mod)
  if(x == "1" & is.null(x_var)){x = "response_ID"
  data$response_ID = 1:nrow(data)} else if(x == "1"){x = x_var}
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)
  abline(mod)
  for(i in 1:nrow(data)){
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))
    abline(v = data[i,x], lty = 2, col = col)
  }
}


#2.2
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

##3  Loading data 
mydata<-read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")



##4  Check and remove the dataset for irregularities
summary(mydata)
str(mydata)
describe(mydata) 

#4.1 #exploration: histogram

plot1<-mydata %>%ggplot() +aes(x = pain) +geom_histogram(bins = 50)
plot2<-mydata %>%ggplot() +aes(x = sex) +geom_bar()
plot3<-mydata %>%ggplot() +aes(x = STAI_trait) +geom_histogram(bins = 50)
plot4<-mydata %>%ggplot() +aes(x = pain_cat) +geom_histogram(bins = 50)
plot5<-mydata %>%ggplot() +aes(x = cortisol_serum) +geom_histogram(bins = 50)
plot6<-mydata %>%ggplot() +aes(x = cortisol_saliva) +geom_histogram(bins = 50)
plot7<-mydata %>%ggplot() +aes(x = mindfulness) +geom_histogram(bins = 50)

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot6,ncol=3)


plot14<-ggplot(data =mydata ,aes(x = age, y = pain,color=sex)) + 
  geom_point() +
  geom_smooth(method = "lm")

plot15<-ggplot(data =mydata ,aes(x = STAI_trait, y = pain,color=sex)) + 
  geom_point() +
  geom_smooth(method = "lm")

plot16<-ggplot(data =mydata ,aes(x = cortisol_saliva, y = pain,color=sex)) + 
  geom_point() +
  geom_smooth(method = "lm")

plot17<-ggplot(data =mydata ,aes(x = cortisol_serum, y = pain,color=sex)) + 
  geom_point() +
  geom_smooth(method = "lm")

plot18<-ggplot(data =mydata ,aes(x = mindfulness, y = pain,color=sex)) + 
  geom_point() +
  geom_smooth(method = "lm")

grid.arrange(plot14,plot15,plot16,plot17,plot18,ncol=3)

mydata %>%
  ggplot() +
  aes(x = sex, y = pain, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.2)

t_test_results = t.test(pain ~ sex, data = mydata)
t_test_results # there is no significant differnce betweent sex in pain

#4.2 remove the data which is seem to be impossible
mydata<-mydata%>%
  filter(age<=150)%>% ##Until now no one can lives longer than 150.
  filter(household_income>=0)%>%  ##The household income need to more than 0.
  filter(STAI_trait>=20,STAI_trait<=80)##The State Trait Anxiety Inventory - T: measures trait anxiety on a scale of 20 to 80



##5 Regression model and test outliers.
#5.1 model 1
mod1<-lm(pain~age+sex,data=mydata)

#5.2 model 2  
mod2<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_saliva+cortisol_serum,data=mydata)

#5.3  Identifying extreme cases with cooks.distance
mod2 %>% plot(which = 4)
mydata %>% slice(c(68, 99, 112))


###6 Assumptions of linear regression
  #6.1 normality
  #6.1.1 QQ plot
  qqPlot(mod2)
  
  #6.1.2 histogram
  residuals_mod2 =enframe(residuals(mod2))
  residuals_mod2 %>%
    ggplot() +aes(x = value) +
    geom_histogram()
  
  #6.1.3 skew and kurtosis
  describe(residuals(mod2))
  ##result show that the model meet the assumption of normality



##6.2  Linearity:the relationship between the outcome variable and the predictor(s) must be linear.
mod2 %>%residualPlots()

##In our case, even though there is minor curvature visible on the STAI_trait, 
##the tests are all non significant, so the linearity assumption seems to hold true for our model.


##6.3  Homoscedasticty
mod2 %>%plot(which = 3)
mod2 %>%ncvTest()# NCV test
mod2 %>%bptest()
#These test indicate that there is not significant heteroscedasticity in mod2


##6.4  No multicollinearity
mod2 %>%vif()
##Because VIF more than 3 in cortisol_saliva and cortisol_serum , multicollinearity may be  problematic.


#6.4.1 figure out what types of multicollinearity

mydata %>%select(pain, cortisol_saliva, cortisol_serum, STAI_trait) %>%pairs.panels(col = "red", lm = T)
##The correlation matrix clearly indicates that the correlation of cortisol_saliva and cortisol_serum is very high. 
##Actually,cortisol_serum and cortisol_saliva are measuring same thing with different measurment, 
##in this anaysis, I will remove the cortisol_serum and keep cortisol_saliva, because cortisol_saliva have a better explanatory power.

boxplot(mydata[,7:8])
t.test(mydata$cortisol_serum,mydata$cortisol_saliva)
#7 refit model and recheck the modal
mod_final<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_saliva,data=mydata)


##7.1 normality
qqPlot(mod_final)

residuals_mod_final <-enframe(residuals(mod_final))
residuals_mod_final %>%
  ggplot() +aes(x = value) +
  geom_histogram()

describe(residuals(mod_final))

#7.2  Linearity
mod_final %>%residualPlots()

##7.3  Homoscedasticty
mod_final %>%plot(which = 3)
mod_final %>%ncvTest()# NCV test
mod_final%>%bptest()

##6.4  No multicollinearity
mod_final %>%vif()


##8 compare model1 and model 2
mod1<-lm(data=mydata, pain~age+sex)
summary(mod1)
AIC(mod1)
confint(mod1) 
lm.beta(mod1)
coef_table(mod1)

mod_final<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_saliva,data=mydata)
summary(mod_final)
AIC(mod_final)
confint(mod_final) 
lm.beta(mod_final)
coef_table(mod_final)


anova(mod1,mod_final) ##the different between two model is significant.
