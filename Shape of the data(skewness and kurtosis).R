library(tidyverse)
library(titanic)
install.packages("moments")
library(moments)
df<-titanic_train
#cleaning n/a from age
age<-skewness(df$Age,na.rm = TRUE)
age

#here skewness is 0.38 which means the curve should be right skeweed
kurt_age<- kurtosis(df$Age,na.rm = TRUE)
kurt_age 
#this is classical kurtosis

excess_kurt_age<-kurt_age-3

#here kurtosis is 0.16 which small amount of excesive data or outliers.
#for fare column
skew_fare<- skewness(df$Fare,na.rm = TRUE)
skew_fare
#skewness is 4.78,indicate highly right skewed.
kurt_fare<- kurtosis(df$Fare,na.rm = TRUE)
kurt_fare
# kurtosis is 36 which means huge part of data are exceeding the typical value.


#histogram of the age data
ggplot(df,aes(x=df$Age)) +
  geom_histogram(aes(y=..density..),bins = 50,
                 color = "darkblue",fill = "yellow",alpha = 0.7)+
  geom_density(colour = "darkred",size=1)+
  geom_vline(xintercept =mean(titanic_train$Age,na.rm=TRUE),color = "lightgreen",size=1.5,linetype = "dashed") +
  geom_vline(xintercept =median(titanic_train$Age,na.rm = TRUE),color="darkblue",size=1.5,linetype = "dashed")+
  labs(
    title ="Distribution of age (lightly Right-Skewed)",
    subtitle = paste("skewness=",round(age,2),
                     "|kurtosis=",round(kurt_age,2)),
    x="age",
    y="density"
  )+
  theme_minimal()
  
  
