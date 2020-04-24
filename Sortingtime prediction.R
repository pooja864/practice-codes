sampledata=read.csv("I:\\Data Science\\simple linear regression\\delivery_time.csv")
View(sampledata)
##install.packages("lattice")
library("lattice")

dotplot(sampledata$Delivery.Time)
dotplot(sampledata$Sorting.Time)

boxplot(sampledata$Delivery.Time,col = "green",horizontal = TRUE)
boxplot(sampledata$Sorting.Time,col = "Magenta",horizontal = TRUE)

hist(sampledata$Delivery.Time,col = "grey",probability = TRUE)
lines(density(sampledata$Delivery.Time, adjust = 2), lty="dotted")

hist(sampledata$Sorting.Time,col="sky blue",probability = TRUE)
lines(density(sampledata$Sorting.Time, adjust = 2),lty= "dotted")

plot(sampledata$Sorting.Time,sampledata$Delivery.Time, main = "scatterplot",
     xlab = "sorting data",ylab = "delivery data",pch=18, col="red")

plot(sampledata$Sorting.Time,sampledata$Delivery.Time)

##----------------------------------------------------------------------------------
## Linear regression performed on raw data

summary(sampledata)
regmodel1<- lm(sampledata$Delivery.Time~sampledata$Sorting.Time)
summary(regmodel1) #0.6823

regmodel1_pred<-predict(regmodel1)
df1<-data.frame(sampledata, "predicted values"=regmodel1_pred)

View(sampledata)
regmodel1_error= df1$Delivery.Time-df1$predicted.values
SSE1=sum(regmodel1_error^2)
SSE1 ##163.6595
df1<-data.frame(sampledata, "predicted values"=regmodel1_pred,regmodel1_error)
View(df1)

##-----------------------------------------------------------------------
##transformation of value x , normalization through sqrt

plot(sampledata$Sorting.Time,sqrt(sampledata$Delivery.Time), main = "scatterplot",pch=19,col="red")

regmodel2<-lm(sqrt(sampledata$Delivery.Time)~sampledata$Sorting.Time)
summary(regmodel2) ## 0.704
regmodel2_pred<-predict(regmodel2)
df2<-data.frame(sampledata, "predicted values"= regmodel2_pred^2)
View(df2)
regmodel2_error=df2$Delivery.Time-df2$X.predicted.values
SSE2=sum(regmodel2_error^2)
SSE2## 170.5111
##----------------------------------------------------------------------------
##transformation using cube root

plot(sampledata$Sorting.Time,(sampledata$Delivery.Time)^1/3)
regmodel3<-lm((sampledata$Delivery.Time)^1/3~sampledata$Sorting.Time)
summary(regmodel3)#0.6823
regmodel3_pred<-predict(regmodel3)
df3<-data.frame(sampledata,"predicated values"= regmodel3_pred^3)
View(df3)
regmodel3_error<-df3$predicated.values-df3$Delivery.Time
SSE3=sum(regmodel3_error^2)
SSE3 #1130438
##------------------------------------------------------------------------------
##transformation using log

plot(sampledata$Sorting.Time,log(sampledata$Delivery.Time))
regmodel4<-lm(log(sampledata$Delivery.Time)~sampledata$Sorting.Time)
summary(regmodel4)#0.7109
regmodel4_pred<-predict(regmodel4)
df4<-data.frame(sampledata,"predicated values"= exp(regmodel4_pred))
View(df4)
regmodel4_error<-df4$predicated.values-df4$Delivery.Time
SSE4=sum(regmodel4_error^2)
SSE4 #181.5465
#---------------------------------------------------------------------------
#transformation using z score
plot(sampledata$Sorting.Time,scale(sampledata$Delivery.Time))
regmodel5<-lm(scale(sampledata$Delivery.Time)~sampledata$Sorting.Time)
summary(regmodel5)#0.6823
regmodel5_pred<-predict(regmodel5)
df5<-data.frame(sampledata,"predicated values"= (regmodel5_pred)*sd(sampledata$Delivery.Time)+mean(sampledata$Delivery.Time))
View(df5)
regmodel5_error<-df5$predicated.values-df5$Delivery.Time
SSE5=sum(regmodel5_error^2)
SSE5 #163.6595
#--------------------------------------------------------
##transformation using both logs
plot(log(sampledata$Sorting.Time),log(sampledata$Delivery.Time))
regmodel6<-lm(log(sampledata$Delivery.Time)~log(sampledata$Sorting.Time))
summary(regmodel6)#0.7722
regmodel6_pred<-predict(regmodel6)
df6<-data.frame(sampledata,"predicated values"= exp(regmodel6_pred))
View(df6)
regmodel6_error<-df6$predicated.values-df6$Delivery.Time
SSE6=sum(regmodel6_error^2)
SSE6 #158.3311
##infrence of the model, "Regmodel6" is considered as best model with high r squared(0.7722)
#and least square error 158.3311
