install.packages("ggplot2")
library(ggplot2)

#Import data set
setwd("c:/Users/RicKeHHHHH/OneDrive/Máy tính/ALY6010")
df <- read.csv("data1.csv")


#Part 1
df$intervention <- ifelse(df$treatment == "Gabapentin", 1, 0)
fit <- lm(outcome ~ wbc + intervention, data = df)
summary(fit)

equation1 <- function(x){coef(fit)[2]*x+coef(fit)[1]}
equation2 <- function(x){coef(fit)[2]*x+coef(fit)[1]+coef(fit)[3]}

ggplot(df,aes(y=outcome,x=wbc,color=treatment))+
  geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[2])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[1])+
  labs(title = "Multiple Regression Scatterplot")+
  xlab("White blood cells")+
  ylab ("Outcome")+
  theme_bw()

#Part 2
df1 <- subset(df, df$intervention == 1)
df2 <- subset(df, df$intervention == 0)

ggplot(df1,aes(y=outcome,x=wbc,color="red"))+
  geom_point(color="red")+
  labs(title = "Single Regression Scatterplot",
       subtitle = "Gabapentin")+
  xlab("White blood cells")+
  ylab ("Outcome")+
  theme_bw()+
  geom_smooth(method = lm, se = T, color = "red", fill = "green")

ggplot(df2,aes(y=outcome,x=wbc))+
  geom_point(color="turquoise")+
  labs(title = "Single Regression Scatterplot",
       subtitle = "Placebo")+
  xlab("White blood cells")+
  ylab ("Outcome")+
  theme_bw()+
  geom_smooth(method = lm, se = T, color = "red", fill = "green")
