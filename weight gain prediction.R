

##read the csv file from folders
sampledata=read.csv("I:\\Data Science\\simple linear regression\\calories_consumed.csv")
##view the data
View(sampledata)
summary(sampledata)
## dotplot and box plot plotters
dotplot(sampledata$Weight.gained..grams.,main="weight gained")
dotplot(sampledata$Calories.Consumed,main="calories consumed")
boxplot(sampledata$Weight.gained..grams.,col = "pink")
boxplot(sampledata$Calories.Consumed,col = "green", horizontal = TRUE)
#histogram plot
hist(sampledata$Weight.gained..grams.,col = "green",probability = TRUE)
##density line with smoother points
lines(density(sampledata$Weight.gained..grams.,adjust = 2), lty="dotted")
hist(sampledata$Calories.Consumed,col = "grey",probability = TRUE)
lines(density(sampledata$Calories.Consumed,adjust = 2), lty="dotted")
qqnorm(sampledata$Weight.gained..grams.)
qqline(sampledata$Weight.gained..grams.)
##Scatter plot
plot(sampledata$Calories.Consumed,sampledata$Weight.gained..grams.)
plot(sampledata$Calories.Consumed,sampledata$Weight.gained..grams.,
main="scatterplot",col="red",xlab = "calories data",ylab = "weight data", pch=19)
#-----------------------------------------------------------------
#Function to run the linear regression equation line with raw data
regmodel<-lm(sampledata$Weight.gained..grams.~ sampledata$Calories.Consumed)
regmodel<-lm(Weight.gained..grams.~Calories.Consumed,data = sampledata)
##summary will give all the factors related to model where we can check the efficency of model
summary(regmodel)  ##0.8968
regmodel_pred<-predict(regmodel)
predata<-data.frame(sampledata,regmodel_pred)
regmodel_error1<- predata$Weight.gained..grams.-predata$regmodel_pred
predata<-data.frame(sampledata,regmodel_pred,regmodel_error1)
View(predata)
SSE<-sum(predata$regmodel_error1^2)
SSE ##149399.7
cor(predata$regmodel_pred,predata$Weight.gained..grams.)
##---------------------------------------------------------------------------------------------------
##transformation of variable,normalization using sqrt
regmodel1<-lm(sqrt(Weight.gained..grams.)~ Calories.Consumed, data=sampledata)
regmodel1
plot(sampledata$Calories.Consumed,sqrt(sampledata$Weight.gained..grams.))
summary(regmodel1)   ##0.9139
regmodel1_pred2<-predict(regmodel1)
df1=data.frame(sampledata,"predvalues"=regmodel1_pred2^2)
View(df1)
regmodel1_error<-df1$Weight.gained..grams.- df1$predvalues
df1=data.frame(sampledata,"predvalues"=regmodel1_pred2^2,regmodel1_error)
SSE1=sum(df1$regmodel1_error^2)
##------------------------------------------------------------------------------------------
##transformation of variable,normalization using log
regmodel2<-lm(log(Weight.gained..grams.)~ Calories.Consumed, data=sampledata)
regmodel2
plot(sampledata$Calories.Consumed,log(sampledata$Weight.gained..grams.))
summary(regmodel2)  ##0.8776
regmodel2_pred3<-predict(regmodel2)
df2=data.frame(sampledata,"predvalues"=exp(regmodel2_pred3))
View(df2)
regmodel2_error<-df2$Weight.gained..grams.- df2$predvalues
df2=data.frame(sampledata,"predvalues"=exp(regmodel2_pred3),regmodel2_error)
SSE2=sum(df2$regmodel2_error^2)
SSE2   ##195085 .2
SSE ##149399.7
SSE1 ##76139.5
summary(regmodel)
abline(regmodel2)
##---------------------------------------------------------------
##transformation of variable. Normalization using sqrt(consumed calories)
regmodel3<-lm(sampledata$Weight.gained..grams.~sqrt(sampledata$Calories.Consumed))
summary(regmodel3) ##0.8567
plot(sqrt(sampledata$Calories.Consumed),sampledata$Weight.gained..grams.)
regmodel3_pred3<-predict(regmodel3)
df3=data.frame(sampledata,"predicated values"=regmodel3_pred3)
regmodel3_error=df3$Weight.gained..grams.-df3$predicated.values
df3=data.frame(sampledata,"predicated values"=regmodel3_pred3,regmodel3_error)
SSE3=sum(regmodel3_error^2)
SSE3 ##207394.1
## regmodel1 will give the best fit line and consider as best model
##with least square error (76239.5) and r square(0.8776)
#------------------------------------------------------------------------------------------


