  #Were the passengers on the Titanic significantly younger or older than
#the general population (30 years old)?"

library(tidyverse)
library(moments)
titanic<-read.csv("Titanic-Dataset.csv")

colSums(is.na(titanic))
#replacing na with the median value of each group

class_medians <- titanic %>%
  group_by(Pclass) %>%
  summarise(median_age = median(Age, na.rm = TRUE))

titanic <- titanic %>%
  left_join(class_medians, by = "Pclass") %>%
  mutate(Age = ifelse(is.na(Age), median_age, Age)) %>%  # We overwrite the original Age column
  select(-median_age)  # Clean up

titanic_clean<-titanic

write_csv(titanic_clean, "titanic_clean.csv")

# now we have clean data set we can use it in hypothesis
head(titanic_clean)
#  #Were the passengers on the Titanic significantly younger or older than
#the general population (30 years old)?"

#RUNNIG T TEST
t.test_result<-t.test(titanic_clean$Age,mu=30)

print(t.test_result)
#data:  titanic_clean$Age
#t = -2.1041, df = 890, p-value = 0.03565
#alternative hypothesis: true mean is not equal to 30
#95 percent confidence interval:
 # 28.19557 29.93725
#sample estimates:
#  mean of x 
#29.06641 


#Because the P-Value is 0.019 (less than 0.05), we can say with confidence:
#"The passengers on the Titanic were statistically significantly younger than
#the general population's average of 30 years old."

#if we change the mean
t.test_result1<-t.test(titanic_clean$Age,mu=32)
print(t.test_result1)

mean_age<-mean(titanic_clean$Age,na.rm = TRUE)



