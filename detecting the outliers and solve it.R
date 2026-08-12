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
  
#fixing/winsorizing the outliers in fare price

cat("Original Fare outliers:", 
    sum(titanic_train$Fare > upper_bound, na.rm = TRUE), "\n")

#original fare outlliers=160

titanic_train<-titanic_train%>%mutate(Fare_capped=ifelse(Fare>upper_bound,upper_bound,Fare))

cat("Capped Fare outliers:", 
    sum(titanic_train$Fare_capped > upper_bound, na.rm = TRUE), "\n")
#capped fare outliers = 0

titanic_train$Fare_capped

#comparing the plot after capped
#before capped
p1<-ggplot(titanic_train,aes(x=Fare))+
  geom_histogram(bins=40,color="green",fill="lightblue",alpha = 1)
  
  #after capped
p2<-ggplot(titanic_train,aes(x=Fare_capped))+
  geom_histogram(aes(y=..density..),bins=40,color="red",fill="lightblue",alpha = 1)+
  geom_density(color="green",size=1.5)

install.packages("gridExtra")
library(gridExtra)

grid.arrange(p1, p2, ncol = 2)

p3<-ggplot(titanic_train,aes(x=Fare))+
  geom_histogram(bins=40,color="green",fill="lightblue",alpha = 1)

p4<-ggplot(titanic_train,aes(x=Fare_capped))+
  geom_histogram(bins=40,color="green",fill="lightblue",alpha = 1)

grid.arrange(p3, p4, ncol = 2)
  

#winsorizing the age 
sum(titanic_train$Age>extreme_old,na.rm = TRUE)

age_cap_limit <- mean_age + 3 * sd_age

titanic<-titanic_train%>%mutate(Age_capped=ifelse(Age > age_cap_limit,extreme_old,Age))
titanic_train$Age_capped


