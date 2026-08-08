library(tidyverse)
library(titanic)
library(moments)
#mean and sd of the age
mean_age<-mean(titanic_train$Age,na.rm = TRUE)
sd_age<-sd(titanic_train$Age,na.rm = TRUE)

#Z score
