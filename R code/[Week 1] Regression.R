### Week 1: Regression ###
## Count Regression on the Number ##
# copied https://www.kaggle.com/code/andrewdk1123/count-regression-on-the-number-of-bicycles/notebook #

# install.packages("tidyverse")
library(tidyverse)

getwd()
bicycles <- read_csv("C:/2024-CodingStudy/Data/nyc-east-river-bicycle-counts.csv")
attach(bicycles)

summary(bicycles) # response: Total

# Transforimg the datatype of Day into chr
bicycles$Day <- weekdays(bicycles$Date)
bicycles$Day <- as.factor(bicycles$Day)
bicycles$Day # Korean
levels(bicycles$Day) <- c("Sunday", "Monday", "Tuesday", "Wednesday",
                          "Thursday", "Friday", "Saturday")
head(bicycles)

# EDA #

library(ggplot2)
ggplot(bicycles, aes(x = Day, y = Total)) +
  geom_boxplot() # total counts per Day
# most on Sat
# weekdays are more disturbed than weekends

class(Precipitation) # chr
unique(Precipitation) # S: snowed, T: trace amount of rain that day

# Making a 2-level factor weather: if rained, "rain", and if not rained, "sunny"
library(dplyr)
bicycles <- bicycles %>%
  mutate(weather = if_else(Precipitation == "0", "sunny", "rain"))
bicycles$weather <- as.factor(bicycles$weather)
levels(bicycles$weather) <- c("sunny", "rain")
summary(bicycles$weather) #sunny: 91, rain: 119

ggplot(bicycles, aes(x = Day, y = Total)) +
  geom_boxplot() +
  facet_grid(~ weather) # total counts per Day separated by weather
# people ride bicycles more on rainy days than on sunny days
# on sunny days, more number of bicycles on Sun
# Conversely, on rainy days, most on Mon

# install.packages("GGally")
library(GGally)
?GGally
ggpairs(bicycles[, c(4:5, 11)])
# some correlation b/w highest temp and lowest temp

# Changing the name of the variables
colnames(bicycles) <- c(names(bicycles)[1:3], "H_Temp", "L_Temp", names(bicycles)[6:12])
# alt: colnames(bicycles)[4:5] <- c("H_Temp", "L_Temp")

# total number of bicycles used by temperature
ggplot(bicycles, aes(x = L_Temp, y = Total, color = weather)) +
  geom_point() + # scatter plot
  geom_smooth(se = FALSE, method = lm) # regression line without standard error(confidence interval)

ggplot(bicycles, aes(x = H_Temp, y = Total, color = weather)) +
  geom_point() +
  geom_smooth(se = FALSE, method = lm)

# no differences b/w the relationships depending on weather

# possion regression
m1 <- glm(Total ~ H_Temp + L_Temp + weather*Day, # interation term of weather & Day
          family = poisson, data = bicycles)
summary(m1) # residual devaince is 68499: if big, not fitted to dataset

# negative binomials regression
#install.packages("MASS")
library(MASS)
m2 <- glm.nb(Total ~ H_Temp + L_Temp + weather*Day,
             data = bicycles)
summary(m2)
pchisq(m2$deviance, m2$df.residual, lower = F) #chisq test for model fit
# 0.2041887: the bigger, more unfitted to dataset

# Dropping low temp due to corr b/w high temp and low temp
# high temp has greater corr coefficient than low has with Total
m3 <- glm.nb(Total ~ H_Temp + weather*Day, data = bicycles)
pchisq(q = deviance(m3) - deviance(m2), df = 1, lower = F)
# if this p-value is greater than 0.05, excluding low temp is not statistically meaningful
pchisq(deviance(m3), df.residual(m3), lower = F)

# Predictions on sunny Sundays
temp <- seq(min(bicycles$H_Temp), max(bicycles$H_Temp), by = 0.01)
n <- length(temp)
n # 4111

new_data <- data.frame(Day = rep("Sunday", n),
                       weather = rep("sunny", n),
                       H_Temp = temp, row.names = paste(temp, "°F"))
head(new_data)

new_data$predictions <- predict(m3, new_data, type = "response")
ggplot(new_data, aes(x = H_Temp, y = predictions)) +
  geom_line() +
  labs(title = "On Sunny Sunday")
