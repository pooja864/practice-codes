sampledata<-read.csv("I:\\Data Science\\simple linear regression\\Salary_Data.csv")
View(sampledata)

dotplot(sampledata$YearsExperience)
dotplot(sampledata$Salary)

boxplot(sampledata$Salary,col = "green",horizontal = TRUE)
boxplot(sampledata$YearsExperience,col = "Magenta",horizontal = TRUE)

hist(sampledata$Salary,col = "grey",probability = TRUE)
lines(density(sampledata$Salary, adjust = 2), lty="dotted")

hist(sampledata$YearsExperience,col="sky blue",probability = TRUE)
lines(density(sampledata$YearsExperience, adjust = 2),lty= "dotted")

plot(sampledata$YearsExperience,sampledata$Salary, main = "scatterplot",
     xlab = "years of exp",ylab = "salary",pch=18, col="red")



##----------------------------------------------------------------------------------
## Linear regression performed on raw data

regmodel1<-lm(sampledata$Salary~sampledata$YearsExperience)
summary(regmodel1) #0.957

regmodel1_pred<-predict(regmodel1)
df1<-data.frame(sampledata , "predicted value"= regmodel1_pred)
View(df1)
regmodel1_error<-df1$Salary-df1$predicted.value
SSE1=sum(regmodel1_error^2)
SSE1 ##938128552

#------------------------------------------------------------

##transformation of both log
regmodel2<-lm(log(sampledata$Salary)~log(sampledata$YearsExperience))
summary(regmodel2)#0.9052

regmodel2_pred<-predict(regmodel2)
df1<-data.frame(sampledata , "predicted value"= exp(regmodel2_pred))
View(df1)
regmodel2_error<-df1$Salary-df1$predicted.value
SSE2=sum(regmodel2_error^2)
SSE2 ##110.113
#--------------------------------------------------------
#transform with dependent variable log
regmodel3<-lm(log(sampledata$Salary)~sampledata$YearsExperience)
summary(regmodel3)#0.932

#-----------------------------transform with sqrt------------------------

regmodel4<-lm(sqrt(sampledata$Salary)~sampledata$YearsExperience)
summary(regmodel4)#0.9498


#-----------------------------transform with sqrt for both the variable------------------------

regmodel5<-lm(sqrt(sampledata$Salary)~sqrt(sampledata$YearsExperience))
summary(regmodel5)#0.9419


#-----------------------------transform with z square -----------------------
plot(sampledata$YearsExperience,scale(sampledata$Salary), main = "scatterplot",
     xlab = "years of exp",ylab = "salary",pch=18, col="red")


regmodel6<-lm(scale(sampledata$Salary)~(sampledata$YearsExperience))
summary(regmodel6)#0.957

regmodel6_pred<-predict(regmodel6)
df1<-data.frame(sampledata , "predicted value"= regmodel6_pred*sd(sampledata$Salary)+mean(sampledata$Salary))
View(df1)
regmodel6_error<-df1$Salary-df1$predicted.value
SSE=sum(regmodel6_error^2)
SSE ##938128552
MAD=sum(abs(regmodel6_error))/nrow(df1)
MAD ##4644
Z#-------------


##infrence of the data set .. regmodel6 is considered as best model with best fit line having 
##high r square(0.957) value with less square error(938128552)





