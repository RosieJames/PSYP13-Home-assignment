data_sample_4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv ")
require(psych)
require(ggplot2)
require(cAIC4)
require(r2glmm)
require(lme4)
require(lmerTest)
require(reshape2)
stdCoef.merMod <- function(object) { 
  sdy <- sd(getME(object, "y")) 
  sdx <- apply(getME(object, "X"), 2, sd) 
  sc <- fixef(object) * sdx/sdy 
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy 
  return(data.frame(stdcoef = sc, stdse = se)) 
  }

#Converting from wide to long
data_sample_5<-data_sample_4
data_sample_5$ID<-factor(data_sample_5$ID)
data_pain_long<-melt(data_sample_5, measure.vars=repeated_variables, variable.name="time", value.name="pain_rating")
data_pain_long<-data_pain_long[order(data_pain_long[,"ID"]),]
data_pain_long$time=as.numeric(data_pain_long$time)
View(data_pain_long)
     
#Check data
describe(data_sample_5)

hist(data_sample_5$pain1)
hist(data_sample_5$pain2)
hist(data_sample_5$pain3)
hist(data_sample_5$pain4)

repeated_variables=c("pain1","pain2","pain3","pain4")
cor(data_sample_5[,repeated_variables])

#Mixed linear models
mod_rep_int = lmer(pain_rating~time+age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva + 
                     (1 | ID), data = data_pain_long) 
mod_rep_slope = lmer(pain_rating ~ time+age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva + 
                     (time | ID), data = data_pain_long)

#Comparing predictions
data_pain_long_withpreds = data_pain_long
data_pain_long_withpreds$pred_int=predict(mod_rep_int)
data_pain_long_withpreds$pres_slope=predict(mod_rep_slope)

#Plots
ggplot(data_pain_long_withpreds, aes(y = pain_rating, x = time, 
                                      group = ID)) + geom_point(size = 3) + geom_line(color = "red", 
                                      aes(y = pred_int, x = time)) + facet_wrap(~ID, ncol = 5)
