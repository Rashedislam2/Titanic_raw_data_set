#mean_median_mode
install.packages("tidyverse")
install.packages("titanic")
library(tidyverse)
library(titanic)
data("titanic_train")
df<-titanic_train
head(df)
summary(df)

mean_age<-mean(df$Age,na.rm = TRUE)
mean_age
median_age<-median(df$Age,na.rm = TRUE)
median_age

get_mode <- function(x) {
  x_clean <- x[!is.na(x)]
  freq_table <- table(x_clean)
  mode_value <- names(freq_table)[which.max(freq_table)]
  return(as.numeric(mode_value))
}
mode_age <-get_mode(df$Age)
mode_age

cat("========================================\n")
cat("DAY 1: CENTRAL TENDENCY FOR AGE\n")
cat("========================================\n")
cat("Mean Age   :", round(mean_age, 2), "\n")
cat("Median Age :", round(median_age, 2), "\n")
cat("Mode Age   :", mode_age, "\n")
cat("========================================\n")

mean_fare<-mean(df$Fare,na.rm = TRUE)
mean_fare
Median_fare<-median(df$Fare,na.rm = TRUE)
Median_fare
mode_fare<-get_mode(df$Fare)
mode_fare

#visualizing the comparision
stats_age <- data.frame(
  Measure = c("Mean", "Median", "Mode"),
  Value = c(mean_age, median_age, mode_age)
)

ggplot(stats_age, aes(x = Measure, y = Value, fill = Measure)) +
  geom_col(width = 0.6) +
  labs(
    title = "Central Tendency of Passenger Age",
    subtitle = "Mean > Median indicates right-skewness",
    y = "Age (Years)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("Mean" = "29.7", "Median" = "28", "Mode" = "24")) +
  geom_text(aes(label = round(Value, 1)), vjust = -0.5, size = 5)


stats_fare <- data.frame(
  Measure1 = c("Mean", "Median", "Mode"),
  Value1 = c(mean_fare, Median_fare, mode_fare)
)

ggplot(stats_age, aes(x = Measure1, y = Value1, fill = Measure1)) +
  geom_col(width = 0.6) +
  labs(
    title = "Central Tendency of Passenger fare",
    subtitle = "Mean > Median indicates right-skewness",
    y = "fare (Years)",
    x = "Measure1"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("Mean" = "32.20421", "Median" = "14.4542", "Mode" = "8.05")) +
  geom_text(aes(label = round(Value, 1)), vjust = -0.5, size = 5)

