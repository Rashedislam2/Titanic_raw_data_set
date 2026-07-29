install.packages("tidyverse")
library(tidyverse)
library(titanic)
data("titanic_train")
df<-titanic_train
head(df)

#calculating variance
var_age<-var(df$Age,na.rm = TRUE)
var_age
sd_age<-sd(df$Age,na.rm = TRUE)
sd_age
#printing the result
cat("========================================\n")
cat("DAY 2: SPREAD FOR AGE\n")
cat("========================================\n")
cat("Variance (Age)      :", round(var_age, 2), "\n")
cat("Standard Deviation  :", round(sd_age, 2), "\n")
cat("========================================\n")

#calculating the interquartile

q1_age<-quantile(df$Age,0.25,na.rm = TRUE)
q3_age<-quantile(df$Age,0.75,na.rm = TRUE)

iqr_age<-q3_age-q1_age
iqr_age

