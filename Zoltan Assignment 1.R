data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv")

require(psych)
require(car)
require(ggplot2)
require(lm.beta)
require(rgl)
require(lmtest)

##data diagnostics
describe(data_sample_1)
describe(data_sample_2)

hist(data_sample_2$pain)
hist(data_sample_2$STAI_trait)
hist(data_sample_2$pain_cat)
hist(data_sample_2$cortisol_serum)
hist(data_sample_2$cortisol_saliva)
hist(data_sample_2$mindfulness)
hist(data_sample_2$weight)

##new dataset
data_sample_2=data_sample_1

#input error
data_sample_2$age[data_sample_2$age==222]=NA 
data_sample_2$age[which(is.na(data_sample_2$age))]<-mean(data_sample_2$age,na.rm=TRUE)

##Create models
Model1<-lm(pain~age+sex, data=data_sample_2)
Model2<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data=data_sample_2)
summary(Model1)
summary(Model2)
AIC(Model1)
AIC(Model2)
confint(Model1)
lm.beta(Model1)
confint(Model2)
lm.beta(Model2)


#Compare models
anova(Model1,Model2)
##(Model 2 significantly improced the fit of the model to the data compared to model 1, F(5,152)=15.33,p<.001)

#Checking outliers and influential cases
data_sample_2$residuals<-resid(Model2)
data_sample_2$standardised.residuals<-rstandard(Model2)
data_sample_2$studentized.residuals<-rstudent(Model2)
data_sample_2$cooks.distance<-cooks.distance(Model2)
data_sample_2$dfbeta<-dfbeta(Model2)
data_sample_2$dffit<-dffits(Model2)
data_sample_2$leverage<-hatvalues(Model2)
data_sample_2$covariance.ratios<-covratio(Model2)

#Looking at residuals
data_sample_2$large.residual<-data_sample_2$standardised.residuals>2|data_sample_2$standardised.residuals< -2
sum(data_sample_2$large.residual)
data_sample_2[data_sample_2$large.residual,c("cooks.distance","leverage","covariance.ratios")]
##all cook's distances are below 1 which means that none of these cases are having an undue influence on the model
## levelerage cut off is 8/160 = 0.05 all cases within 3 times the boundary and one case 2 times.
##Upper CVR = 1+3xaverage leverage (1.15)
##Lower CVR = 1-3xaverage leverage (0.85)

#Assessing assumption of independence
dwt(Model2) ##assumption met as it is close to 2 and the p-value is not significant

#Assessising assumption of multicollinearity
vif(Model2) ##all below 10 so good
1/vif(Model2) ##tolerance, all but one above 0.2, but it is close to 0.2
mean(vif(Model2)) ##mean vif - not substantially greater than 1, so good

#Looking at the residuals
hist(data_sample_2$studentized.residuals)
hist( x = residuals( Model2 ), xlab="Value of Residuals", main="", breaks=20)
plot(Model2,which=2)
describe(residuals(Model2))
residualPlots(Model2)
plot(Model2, which=3)
ncvTest(Model2)
bptest(Model2)
