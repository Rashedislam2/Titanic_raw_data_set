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


# Boxplot for Age

ggplot(df, aes(y = Age)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Boxplot of Passenger Ages",
    subtitle = "Shows Median, IQR, and Outliers",
    y = "Age"
  ) +
  theme_minimal()+
  annotate("text", x = 0.5, y = q1_age, label = paste("Q1 =", round(q1_age, 1)), 
           color = "darkgreen", size = 3.5, vjust = 1.5) +
  annotate("text", x = 0.5, y = median(df$Age, na.rm = TRUE), 
           label = paste("Median =", round(median(df$Age, na.rm = TRUE), 1)), 
           color = "darkorange", size = 2.5, vjust = 1.5) +
  annotate("text", x = 0.5, y = q3_age, label = paste("Q3 =", round(q3_age, 1)), 
           color = "darkgreen", size = 3.5, vjust = -1.5)
  
ggplot(df, aes(y = Age)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Boxplot of Passenger Ages",
    subtitle = "Shows Median, IQR, and Outliers",
    y = "Age"
  ) +
  theme_minimal() +
  annotate("text", x = 0.5, y = q1_age, label = paste("Q1 =", round(q1_age, 1)), 
           color = "darkgreen", size = 3.5, vjust = 1.5) +
  annotate("text", x = 0.5, y = median(df$Age, na.rm = TRUE), 
           label = paste("Median =", round(median(df$Age, na.rm = TRUE), 1)), 
           color = "darkorange", size = 3.5, vjust = 1.5) +
  annotate("text", x = 0.5, y = q3_age, label = paste("Q3 =", round(q3_age, 1)), 
           color = "darkgreen", size = 3.5, vjust = -1.5)

#outliers for age
upper_bound_age<-q3_age+1.5*iqr_age
lower_bound_age<-q1_age-1.5*iqr_age
#which means values greater than 65 and lower than -7 are outliers

#outliers
extreme_old <-titanic_train%>%filter(Age>64.8)%>%select(Age)
extreme_old

extreme_young <-titanic_train%>%filter(Age< -6.69)%>%select(Age)
extreme_young
