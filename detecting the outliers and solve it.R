library(tidyverse)
library(titanic)
library(moments)
#detecting the outliers by IQR in fare column because they are assymetric
q1<-quantile(titanic_train$Fare,0.25,na.rm = TRUE)
q3<-quantile(titanic_train$Fare,0.75,na.rm=TRUE)
Iqr_fare<-q3-q1

lower_bound<-q1-1.5*Iqr_fare
upper_bound<-q3+1.5*Iqr_fare

fare_outliers <- titanic_train %>%
  filter(Fare < lower_bound | Fare > upper_bound) %>%
  select(Fare)
fare_outliers

#detecting outliers by z score as age column are symmetric

mean_age<-mean(titanic_train$Age,na.rm = TRUE)
sd_age <- sd(titanic_train$Age,na.rm=TRUE)

titanic_train<-titanic_train%>%mutate(z_score_manual=(titanic_train$Age-mean_age)/sd_age)

extreme_old<-titanic_train %>% filter(z_score_manual > 3) %>% select(Age,z_score_manual)

extreme_young<-titanic_train %>% filter(z_score_manual < -3) %>% select(Age,z_score_manual)
extreme_young

cat("Passengers older than 3 SDs above mean:\n")
print(extreme_old)

cat("\nPassengers younger than 3 SDs below mean:\n")
print(extreme_young)

#ploting the zscore
ggplot(titanic_train,aes(x=z_score_manual))+
  geom_histogram(aes(y=..density..),bins = 40,color="black",fill="yellow",alpha = 1)+
  geom_density(color="red",size=1.5)+
  geom_vline(xintercept = 0,color="black",size=1.5,linetype = "dashed")+
  geom_vline(xintercept =c(-2,2),color="green",size=1.5,linetype = "dashed")+
  geom_vline(xintercept = c(-3,3),color="blue",size=1.5,linetype = "dashed")+
  
  labs(
    title = "Distribution of zscore",
    subtitle="Green line= ±2 sd (unusal)|Blue lines= ±3 sd(outliers)",
    x="z_score",
    y= "density"
  )+
  theme_minimal()
# we can easily see that outliers in histogram
  
#fixing the outliers in fare price



  





