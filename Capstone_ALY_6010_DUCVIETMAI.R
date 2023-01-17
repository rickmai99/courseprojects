install.packages("tidyverse")
install.packages("ggplot2")
install.packages("MASS")
install.packages("BDSA")
install.packages("gmodels")
library(tidyverse)
library(ggplot2)
library(MASS)
library(BSDA)
library(gmodels)

#Import data set
setwd("c:/Users/RicKeHHHHH/OneDrive/Máy tính/ALY6010")
df <- read.csv("cost-of-living.csv")
df <- na.omit(df)

summary(df$x33)
sd(df$x33)
summary(df$x36)
sd(df$x36)
summary(df$x42)
sd(df$x42)
summary(df$x54)
sd(df$x54)
summary(df$x48)
sd(df$x48)
summary(df$x49)
sd(df$x49)

#Hypothesis tests
(t1 <- z.test(df$x48, sigma.x = sd(df$x48),
                   conf.level = 0.95, mu = 0))
if (t1$p.value < 0.05) {
  print("Reject the null hypothesis. We have enough statistical evidence to conclude the mean price of apartment in City Centre is different from 0.")
} else {
  print("Not reject the null hypothesis")
}

(t2 <- z.test(df$x42, sigma.x = sd(df$x42),
                   conf.level = 0.95, mu = 0))
if (t2$p.value < 0.05) {
  print("Reject the null hypothesis. We have enough statistical evidence to conclude the mean price of kindergarden is different from 0.")
} else {
  print("Not reject the null hypothesis")
}

(t3 <- z.test(df$x36, sigma.x = sd(df$x36),
                   conf.level = 0.95, mu = 0))
if (t3$p.value < 0.05) {
  print("Reject the null hypothesis. We have enough statistical evidence to conclude the mean price of basic utilities is different from 0.")
} else {
  print("Not reject the null hypothesis")
}

(t4 <- z.test(df$x33, sigma.x = sd(df$x33),
              conf.level = 0.95, mu = 0))
if (t4$p.value < 0.05) {
  print("Reject the null hypothesis. We have enough statistical evidence to conclude the mean price of gasoline is different from 0.")
} else {
  print("Not reject the null hypothesis")
}

(t5 <- z.test(df$x54, sigma.x = sd(df$x54),
              conf.level = 0.95, mu = 0))
if (t5$p.value < 0.05) {
  print("Reject the null hypothesis. We have enough statistical evidence to conclude the mean average monthly salary is different from 0.")
} else {
  print("Not reject the null hypothesis")
}

(t6 <- z.test(df$x49, sigma.x = sd(df$x49),
              conf.level = 0.95, mu = 0))
if (t6$p.value < 0.05) {
  print("Reject the null hypothesis. We have enough statistical evidence to conclude the mean price of apartment outside of Centre is different from 0.")
} else {
  print("Not reject the null hypothesis")
}

#Correlation tests
(a <- cor.test(df$x49, df$x48))
if (a$p.value < 0.05) {
  print("Reject the null hypothesis. We have enough statistical evidence to conclude the correlation is different from 0.")
} else {
  print("Not reject the null hypothesis.")
}
(b <- cor.test(df$x54, df$x42))
if (b$p.value < 0.05) {
  print("Reject the null hypothesis. We have enough statistical evidence to conclude the correlation is different from 0.")
} else {
  print("Not reject the null hypothesis.")
}
(c <- cor.test(df$x33, df$x36))
if (c$p.value < 0.05) {
  print("Reject the null hypothesis. We have enough statistical evidence to conclude the correlation is different from 0.")
} else {
  print("Not reject the null hypothesis.")
}

#Linear regression models
summary(lm(df$x48 ~ df$x49))
ggplot(df, aes(y = x48, x = x49))+
  geom_point(color = "black")+
  labs(title = "Single Regression Scatterplot",
       subtitle = "Apartment In & Outside City Centre")+
  xlab("Price of apartment outside of Centre (USD)")+
  ylab ("Price of apartment in City Centre (USD)")+
  theme_bw()+
  geom_smooth(method = lm, se = T, color = "red", fill = "green")

summary(lm(df$x42 ~ df$x54))
ggplot(df, aes(y = x42, x = x54))+
  geom_point(color = "black")+
  labs(title = "Single Regression Scatterplot",
       subtitle = "Average Monthly Salary & Kindergarden's Costs")+
  xlab("Average Monthly Salary (USD)")+
  ylab ("Kindergarden's Costs (1 Child) (USD)")+
  theme_bw()+
  geom_smooth(method = lm, se = T, color = "red", fill = "green")

summary(lm(df$x36 ~ df$x33))
ggplot(df, aes(y = x36, x = x33))+
  geom_point(color = "black")+
  labs(title = "Single Regression Scatterplot",
       subtitle = "Gasoline & Basic Utilities")+
  xlab("Price of Gasoline (1L) (USD)")+
  ylab ("Price of Basic Utilities (USD)")+
  theme_bw()+
  geom_smooth(method = lm, se = T, color = "red", fill = "green")



