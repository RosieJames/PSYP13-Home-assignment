data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv")
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv")

require(psych)
require(car)
require(ggplot2)
require(lm.beta)
require(rgl)
require(lmtest)

#Previous regression model
data_sample_2=data_sample_1
data_sample_2$age[data_sample_2$age==222]=NA 
data_sample_2$age[which(is.na(data_sample_2$age))]<-mean(data_sample_2$age,na.rm=TRUE)
TheorybasedModel<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data=data_sample_2)

#New regression model
Model3<-lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+weight, data=data_sample_2)

#Data diagnostics
hist(data_sample_2$pain)
hist(data_sample_2$STAI_trait)
hist(data_sample_2$pain_cat)
hist(data_sample_2$cortisol_serum)
hist(data_sample_2$cortisol_saliva)
hist(data_sample_2$mindfulness)
hist(data_sample_2$weight)

#Looking at residuals because of added weight variable and removed cortisol saliva variable
data_sample_2$residuals<-resid(Model3)
data_sample_2$standardised.residuals<-rstandard(Model3)
data_sample_2$studentized.residuals<-rstudent(Model3)
data_sample_2$cooks.distance<-cooks.distance(Model3)
data_sample_2$dfbeta<-dfbeta(Model3)
data_sample_2$dffit<-dffits(Model3)
data_sample_2$leverage<-hatvalues(Model3)
data_sample_2$covariance.ratios<-covratio(Model3)
data_sample_2$large.residual<-data_sample_2$standardised.residuals>2|data_sample_2$standardised.residuals< -2
sum(data_sample_2$large.residual)
data_sample_2[data_sample_2$large.residual,c("cooks.distance","leverage","covariance.ratios")]

dwt(Model3)
vif(Model3)

hist(data_sample_2$studentized.residuals)
hist( x = residuals( Model3 ), xlab="Value of Residuals", main="", breaks=20)
plot(Model3,which=2)
describe(residuals(Model3))
residualPlots(Model3)
plot(Model2, which=3)
ncvTest(Model3)
bptest(Model3)


#backwards regression model
step(object=Model3, direction="backward")

#Backward regression model
BackwardModel<-lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum, data=data_sample_2)
summary(BackwardModel)
confint(BackwardModel)
lm.beta(BackwardModel)

#Compare models
anova(BackwardModel,Model3)
AIC(Model3)
anova(BackwardModel,TheorybasedModel)
AIC(TheorybasedModel)
AIC(BackwardModel)

#train set and test set
training_set=data_sample_3[1:80,]
test_set=data_sample_3[81:160,]

Mod_back_train=lm(pain~age+sex+pain_cat+mindfulness+cortisol_serum,data=training_set)
Mod_theory_train=lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data=training_set)
pred_train_back=predict(Mod_back_train, training_set)
pred_train_theo=predict(Mod_theory_train, training_set)

RSS_train = sum((training_set[,"pain"] - pred_train_back)^2)
RSS_train_theo = sum((training_set[,"pain"]- pred_train_theo)^2)
RSS_train
RSS_train_theo

#Testing
pred_test_theo<- predict (Mod_theory_train, test_set)
pred_test_back <- predict(Mod_back_train, test_set)
RSS_test_theo = sum((test_set[,"pain"] - pred_test_theo)^2)
RSS_test_back=sum((test_set[,"pain"]-pred_test_back)^2)
RSS_test_theo
RSS_test_back

#Model accuracy
summary(pred_test_back)$adj.r.squared
summary(pred_test_theo)$adj.r.squared
