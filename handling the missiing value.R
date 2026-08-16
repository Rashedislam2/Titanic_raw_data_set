library(tidyverse)
titanic<-read.csv("Titanic-Dataset.csv")
titanic
#checking column with na 
colSums(is.na(titanic))

#here age cloumn has 177 missing values
#simple median imputation
global_median_age <-median(titanic$Age,na.rm = TRUE)


titanic<-titanic %>% mutate(Age_filled_simple = ifelse(is.na(Age), global_median_age ,Age))
sum(is.na(titanic$Age_filled_simple))

#group imputation

class_medians<-titanic%>%group_by(Pclass)%>% summarise(median_age=median(Age,na.rm = TRUE))
print(class_medians)

titanic<-titanic%>%
  left_join(class_medians,by= "Pclass")%>%
  mutate(Age_filled_group=ifelse(is.na(Age),median_age,Age))%>% select(-median_age)

sum(is.na(titanic$Age_filled_group))

#visualizing before and after imputation
#before
p1<-ggplot(titanic,aes(x=Age))+
  geom_histogram(bins = 40,color="darkblue",fill = "lightcoral",alpha = 1)+
  labs(
    title = "original age with NA"
  )+
  theme_minimal()
#after

p2<-ggplot(titanic,aes(x=Age_filled_group))+
  geom_histogram(bins = 40,color="darkblue",fill = "lightcoral",alpha = 1)+
  labs(
    title = "Age filled by Pclass"
  )+
  theme_minimal()
library(gridExtra)

grid.arrange(p1,p2,ncol=2)


