## Andy McBurney
## COGS 50.08: Modeling Mind and Behavior
## Survey Data

library(tidyverse)
library(MASS)

#read in data
## NOTE: personality trait questions have already been correctly recoded in Qualtrics accoding to 
## https://www.colby.edu/psych/wp-content/uploads/sites/50/2013/08/bfi2s-form.pdf
## DO NOT reverse-key items, it has already been done
data <- (read.csv("/Users/andrewmcburney/Desktop/COGS_50.08/COGS+50.08_March+12,+2023_14.37.csv")[-c(1:2),-c(10:23)])

#recode the third impulse buy question
data <- data%>%
  mutate(Q2.1_3 = case_when(
  Q2.1_3 == 1 ~ 5,
  Q2.1_3 == 2 ~ 4,
  Q2.1_3 == 3 ~ 3,
  Q2.1_3 == 4 ~ 2,
  Q2.1_3 == 5 ~ 1))
  
#replace empty observations in Q2.6 with 0
data <- replace(data, 'Q2.6',ifelse(data$Q2.6 == "", 0, data$Q2.6)) 

#make personality trait questions integers
data[,c(10:57,59:60)] <- sapply(data[,c(10:57,59:60)],as.integer) 

#create trait variables according to https://www.colby.edu/psych/wp-content/uploads/sites/50/2013/08/bfi2s-form.pdf
data2 <- data%>%
  group_by(ResponseId)%>%
  mutate(extraversion = mean(c(Q4.1, Q4.6, Q4.11, Q4.16, Q4.21, Q4.26)))%>%
  mutate(agreeableness = mean(c(Q4.2, Q4.7, Q4.12, Q4.17, Q4.22, Q4.27)))%>%
    mutate(conscientiousness = mean(c(Q4.3, Q4.8, Q4.13, Q4.18, Q4.23, Q4.28)))%>%
  mutate(neg_emotionality = mean(c(Q4.4, Q4.9, Q4.14, Q4.19, Q4.24, Q4.24)))%>%
  mutate(open_mindedness = mean(c(Q4.5, Q4.10, Q4.15, Q4.20, Q4.25, Q4.30)))%>%
  mutate(impulse_buy_score = mean(c(Q2.1_1, Q2.1_2, Q2.1_3, Q2.1_4, Q2.1_5)))

data3 <- na.omit(data2, cols=c(seq(Q2.1_1: Q2.2_8), seq(Q4.1 : Q4.30)))%>%
  filter_all(~ !(. %in% c(-99, 99)))%>%
  mutate(seconds = as.integer(Duration..in.seconds.))

avg_data <- data3%>%
  ungroup()%>%
  dplyr::summarize(avg_ext = mean(extraversion),
            avg_agr = mean(agreeableness),
            avg_con = mean(conscientiousness),
            avg_neg = mean(neg_emotionality),
            avg_ope = mean(open_mindedness),
            avg_ipm = mean(impulse_buy_score),
            sd_ext = sd(extraversion),
            sd_agr = sd(agreeableness),
            sd_con = sd(conscientiousness),
            sd_neg = sd(neg_emotionality),
            sd_ope = sd(open_mindedness),
            sd_ipm = sd(impulse_buy_score))

#average time to finish survey
avg_time <- as.numeric(data3%>%
  filter(seconds <= 1000)%>%
  group_by(Finished)%>%
  dplyr::summarize(time = mean(seconds)/60)%>%
  dplyr::select(time))
avg_time

#correlation matrix of 5 personality traits
cor_matrix <- cor(data3[,c(69:74)])
cor_matrix

#check for nornal distributions using histogram plots
{
  ggplot()+
  geom_histogram(data = data3, aes(x = Q2.1_1, y = after_stat(count)), fill = 'black', bins = 5)+
    theme_pubr()
    ggplot()+
  geom_histogram(data = data3, aes(x = Q2.1_2, y = after_stat(count)), fill = 'black', bins = 5)+
    theme_pubr()
      ggplot()+
  geom_histogram(data = data3, aes(x = Q2.1_3, y = after_stat(count)), fill = 'black', bins = 5)+
    theme_pubr()
        ggplot()+
  geom_histogram(data = data3, aes(x = Q2.1_4, y = after_stat(count)), fill = 'black', bins = 5)+
    theme_pubr()
          ggplot()+
  geom_histogram(data = data3, aes(x = Q2.1_5, y = after_stat(count)), fill = 'black', bins = 5)+
    theme_pubr()
          
ggplot()+
  geom_histogram(data = data3, aes(x = impulse_buy_score), bins = 40)
ggplot()+
  geom_histogram(data = data3, aes(x = extraversion), bins = 40)
ggplot()+
  geom_histogram(data = data3, aes(x = agreeableness), bins = 40)
ggplot()+
  geom_histogram(data = data3, aes(x = conscientiousness), bins = 40)
ggplot()+
  geom_histogram(data = data3, aes(x = neg_emotionality), bins = 40)
ggplot()+
  geom_histogram(data = data3, aes(x = open_mindedness), bins = 40)
}

#linear model for total impulse buying score
data_glm <- lm(impulse_buy_score ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness, data = data3)

summary(data_glm)                  
anova(data_glm)

#plot linear regression model tests
{
par(mfrow = c(2, 4))
hist(residuals(data_glm))

# Plot residuals vs. fitted values
plot(data_glm$fitted.values, residuals(data_glm), xlab = "Fitted values", ylab = "Residuals")

# Plot normal probability plot
qqnorm(residuals(data_glm))
qqline(residuals(data_glm))

# Check assumptions of normality and linearity using residual vs predictor variable plots
plot(data3$extraversion, residuals(data_glm), xlab = "extraversion", ylab = "Residuals")
plot(data3$agreeableness, residuals(data_glm), xlab = "agreeableness", ylab = "Residuals")
plot(data3$conscientiousness, residuals(data_glm), xlab = "conscientiousness", ylab = "Residuals")
plot(data3$neg_emotionality, residuals(data_glm), xlab = "neg_emotionality", ylab = "Residuals")
plot(data3$open_mindedness, residuals(data_glm), xlab = "open_mindedness", ylab = "Residuals")
}
#export plots
{
jpeg("/Users/andrewmcburney/Desktop/COGS_50.08/linear_regression_tests.jpeg", width = 1200, height = 600)
par(mfrow = c(2, 4))
hist(residuals(data_glm))

# Plot residuals vs. fitted values
plot(data_glm$fitted.values, residuals(data_glm), xlab = "Fitted values", ylab = "Residuals")

# Plot normal probability plot
qqnorm(residuals(data_glm))
qqline(residuals(data_glm))

# Check assumptions of normality and linearity using residual vs predictor variable plots
plot(data3$extraversion, residuals(data_glm), xlab = "extraversion", ylab = "Residuals")
plot(data3$agreeableness, residuals(data_glm), xlab = "agreeableness", ylab = "Residuals")
plot(data3$conscientiousness, residuals(data_glm), xlab = "conscientiousness", ylab = "Residuals")
plot(data3$neg_emotionality, residuals(data_glm), xlab = "neg_emotionality", ylab = "Residuals")
plot(data3$open_mindedness, residuals(data_glm), xlab = "open_mindedness", ylab = "Residuals")
dev.off()
}
#variance inflation factor multicolinearity test
vif(data_glm)
#low multicolinearity! linear regression model is stable.

#linear model for Q2.1_1
data_glm2.1_1 <- lm(Q2.1_1 ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness, data = data3)

summary(data_glm2.1_1)                  
anova(data_glm2.1_1)
vif(data_glm2.1_1)

#linear model for Q2.1_2
data_glm2.1_2 <- lm(Q2.1_2 ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness, data = data3)

summary(data_glm2.1_2)                  
anova(data_glm2.1_2)
vif(data_glm2.1_2)

#linear model for Q2.1_3
data_glm2.1_3 <- lm(Q2.1_3 ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness, data = data3)

summary(data_glm2.1_3)                  
anova(data_glm2.1_3)
vif(data_glm2.1_3)

#linear model for Q2.1_4
data_glm2.1_4 <- lm(Q2.1_4 ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness, data = data3)

summary(data_glm2.1_4)                  
anova(data_glm2.1_4)
vif(data_glm2.1_4)

#linear model for Q2.1_5
data_glm2.1_5 <- lm(Q2.1_5 ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness, data = data3)

summary(data_glm2.1_5)                  
anova(data_glm2.1_5)
vif(data_glm2.1_5)


