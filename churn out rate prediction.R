sampledata<-read.csv("I:\\Data Science\\simple linear regression\\emp_data.csv")
View(sampledata)

dotplot(sampledata$Salary_hike)
dotplot(sampledata$Churn_out_rate)

boxplot(sampledata$Salary_hike,col = "green",horizontal = TRUE)
boxplot(sampledata$Churn_out_rate,col = "Magenta",horizontal = TRUE)

hist(sampledata$Churn_out_rate,col = "grey",probability = TRUE)
lines(density(sampledata$Salary_hike, adjust = 2), lty="dotted")

hist(sampledata$Salary_hike,col="sky blue",probability = TRUE)
lines(density(sampledata$Salary_hike, adjust = 2),lty= "dotted")

plot(sampledata$Salary_hike,sampledata$Churn_out_rate, main = "scatterplot",
     xlab = "salary hike",ylab = "churn out rate",pch=18, col="red")



##----------------------------------------------------------------------------------
## Linear regression performed on raw data

regmodel1<-lm(sampledata$Churn_out_rate~sampledata$Salary_hike)
summary(regmodel1)#0.8312
#------------------------------------------------------------

##transformation of both log
regmodel2<-lm(log(sampledata$Churn_out_rate)~log(sampledata$Salary_hike))
summary(regmodel2)#0.8891

regmodel2_pred<-predict(regmodel2)
df1<-data.frame(sampledata , "predicted value"= exp(regmodel2_pred))
View(df1)
regmodel2_error<-df1$Churn_out_rate-df1$predicted.value
SSE=sum(regmodel2_error^2)
SSE ##110.113
#--------------------------------------------------------
#transform with dependent variable log
regmodel3<-lm(log(sampledata$Churn_out_rate)~sampledata$Salary_hike)
summary(regmodel3)#0.8735

#-----------------------------transform with sqrt------------------------

regmodel4<-lm(sqrt(sampledata$Churn_out_rate)~sampledata$Salary_hike)
summary(regmodel4)#0.853


#-----------------------------transform with sqrt for both the variable------------------------

regmodel5<-lm(sqrt(sampledata$Churn_out_rate)~sqrt(sampledata$Salary_hike))
summary(regmodel5)#0.8614


#-----------------------------transform with z square -----------------------

regmodel6<-lm(scale(sampledata$Churn_out_rate)~(sampledata$Salary_hike))
summary(regmodel6)#0.8312

##infrence of the data set .. regmodel2 is considered as best model with best fit line having 
##high r square(0.8891) value with less square error(110.113)





