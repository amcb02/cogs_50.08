## Andy McBurney
## COGS 50.08: Modeling Mind and Behavior
## Survey Data

#libraries
{
library(tidyverse)
library(cowplot)
library(MASS)
library(rms)
library(vip)
library(scales)
library(knitr)
library(ggpubr)
}

#read in data
## NOTE: personality trait questions have already been correctly recoded in Qualtrics accoding to 
## https://www.colby.edu/psych/wp-content/uploads/sites/50/2013/08/bfi2s-form.pdf
## DO NOT reverse-key items, it has already been done
data <- (read.csv("/Users/andrewmcburney/Desktop/COGS_50.08/COGS+50.08_March+13,+2023_21.50.csv")[-c(1:2),-c(10:23)])

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
  filter(Q5.1 != 2021)%>%
  filter(Finished == 1)%>%
  mutate(seconds = as.integer(Duration..in.seconds.))%>%
    mutate(income = factor(case_when(
    Q2.3 <= 50 ~ 1,
    Q2.3 <= 100 & Q2.3 >50 ~ 2,
    Q2.3 <= 200 & Q2.3 >100 ~ 3,
    Q2.3 > 200 ~ 4), levels = seq(1,4,1)))

mean_data <- data3%>%
  ungroup()%>%
  dplyr::summarize(ext = mean(extraversion),
            agr = mean(agreeableness),
            con = mean(conscientiousness),
            neg = mean(neg_emotionality),
            ope = mean(open_mindedness),
            ipm = mean(impulse_buy_score))

sd_data <- data3%>%
  ungroup()%>%
  dplyr::summarize(ext = sd(extraversion),
            agr = sd(agreeableness),
            con = sd(conscientiousness),
            neg = sd(neg_emotionality),
            ope = sd(open_mindedness),
            ipm = sd(impulse_buy_score))

#descriptive stat table
descriptive_stats_vars <- rbind(count = nrow(data3), mean = round(mean_data, 2), sd = round(sd_data, 2))%>%t()%>%data.frame()
#rename rows
rownames(descriptive_stats_vars) <- c("Extraversion","Agreeableness", "Conscientiousness",   "Negative Emotionality", "Open-Mindedness", "Impulsive Buying Score")
#output to Latex
kable(descriptive_stats_vars, format = "latex")

ggplot(descriptive_stats_vars)+
geom_errorbar(aes(x = factor(rownames(descriptive_stats_vars)), y = mean, ymin = mean - sd, ymax = mean + sd), width = 0.2)+
  geom_point(aes(x = factor(rownames(descriptive_stats_vars)), y = mean), size = 4)+
  ylim(1,5)+
  labs(title = "Mean and Standard Deviations of\nPersonality Traits and Impulsive Buying Score")+
  xlab("Variable")+
  ylab("Score")+
  scale_x_discrete(labels = c("Agreeableness", "Conscientiousness", "Extraversion", "Impulsive\nBuying\nScore", "Negative\nEmotionality", "Open\nMindedness"))+
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  coord_flip()

income_groups <- data3%>%
  group_by(income)%>%
  count()
colnames(income_groups) <- c("Disposable Income", "# Students")
income_groups$`Disposable Income` <-c("<$50", "$50-$100", "$100-$200", ">$200")


kable(income_groups, format = 'latex')

ggplot(data3%>%group_by(income)%>%count())+
  geom_col(aes(x = income, y = n), fill = 'black')+
      geom_label(aes(x=income, y=n/2, label = n))+
      geom_label(aes(x=income, y=0, label = percent(n/nrow(data3))))+
  labs(title = "Weekly Disposable Income Groups")+
  xlab("Disposable Income")+
  ylab("# of Students")+
  scale_x_discrete(labels = c("<$50", "$50-$100", "$100-$200", ">$200"))+
theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'))

  


  

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

cor_matrix2 <- cor(data3[,c("Q5.3","Q2.2_2", "Q2.2_3","extraversion", "agreeableness", "conscientiousness", "neg_emotionality", "open_mindedness", "impulse_buy_score")])
cor_matrix2
#check for nornal distributions using histogram plots
{
  
  ggplot(data3%>%
    group_by(Q2.1_1)%>%
    count())+
  geom_col(aes(x = Q2.1_1, y=n), fill = 'black')+
    geom_label(aes(x=Q2.1_1, y=n/2, label = n))+
      scale_y_continuous(breaks = seq(0,150, 10))+
  theme_cowplot()+
  labs(title = "Count of Responses for\n 'I go online shopping to change my mood\n or take my mind off of other things.'")+
  xlab("Answer")+
  ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5))
  
  ggplot(data3%>%
    group_by(Q2.1_2)%>%
    count())+
  geom_col(aes(x = Q2.1_2, y=n), fill = 'black')+
    geom_label(aes(x=Q2.1_2, y=n/2, label = n))+
    scale_y_continuous(breaks = seq(0,150, 10))+
  theme_cowplot()+
  labs(title = "Count of Responses for\n 'I feel a sense of excitement when\nI make an impulse purchase online.'")+
  xlab("Answer")+
  ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5))
  
  ggplot(data3%>%
    group_by(Q2.1_3)%>%
    count())+
  geom_col(aes(x = Q2.1_3, y=n), fill = 'black')+
    geom_label(aes(x=Q2.1_3, y=n/2, label = n))+
    scale_y_continuous(breaks = seq(0,150, 10))+
  theme_cowplot()+
  labs(title = "Count of Responses for\n 'After I make an online impulse\npurchase I feel regret.'")+
  xlab("Answer")+
  ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5))
  
  ggplot(data3%>%
    group_by(Q2.1_4)%>%
    count())+
  geom_col(aes(x = Q2.1_4, y=n), fill = 'black')+
    geom_label(aes(x=Q2.1_4, y=n/2, label = n))+
      scale_y_continuous(breaks = seq(0,150, 10))+
  theme_cowplot()+
  labs(title = "Count of Responses for\n 'I have difficulty controlling my urge\nto buy when I see a good offer online.'")+
  xlab("Answer")+
  ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5))
    
  ggplot(data3%>%
    group_by(Q2.1_5)%>%
    count())+
  geom_col(aes(x = Q2.1_5, y=n), fill = 'black')+
    geom_label(aes(x=Q2.1_5, y=n/2, label = n))+
    scale_y_continuous(breaks = seq(0,150, 10))+
  theme_cowplot()+
  labs(title = "Count of Responses for\n 'When I see a good deal online,\nI tend to buy more than that I intended to buy.'")+
  xlab("Answer")+
  ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5))
          
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

#count by class year
class_year_count_responses <- data%>%
  group_by(Q5.1)%>%
  count()

total_responses <- sum(class_year_count_responses$n)

ggplot(class_year_count_responses)+
  geom_col(aes(x=Q5.1, y=n), fill = 'black')+
  geom_label(aes(x=Q5.1, y=n/2, label = n))+
  geom_label(aes(x=Q5.1, y=20, label = percent(n/sum(class_year_count_responses$n))))+
  scale_y_continuous(breaks = seq(0,150, 10))+
  scale_x_discrete(limits = seq(2021,2026,1))+
  theme_cowplot()+
  labs(title = "Count of Total Responses by Class Year")+
  xlab("Class Year")+
  ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5))

class_year_count_filtered <- data3%>%
  group_by(Q5.1)%>%
  count()
colnames(class_year_count_filtered) <- c("Class Year", "# Useable Responses")
kable(class_year_count_filtered, format = "latex")
total_filtered_responses <- sum(class_year_count_filtered$n)

ggplot(class_year_count_filtered)+
  geom_col(aes(x=Q5.1, y=n), fill = 'black')+
  geom_label(aes(x=Q5.1, y=n/2, label = n))+
  geom_label(aes(x=Q5.1, y=20, label = percent(n/sum(class_year_count_filtered$n))))+
  scale_y_continuous(breaks = seq(0,120, 10))+
  theme_cowplot()+
  labs(title = "Count of Filtered Responses by Class Year")+
  xlab("Class Year")+
  ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5))

#no scientific notation
options(scipen = 999)

#linear model for total impulse buying score
data_glm_null <- glm(impulse_buy_score ~ 1, data = data3)
summary(data_glm_null)

data_glm <- glm(impulse_buy_score ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness, data = data3)
summary(data_glm)                  
anova(data_glm)
vip(data_glm)

data_glm2 <- glm(impulse_buy_score ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness + Q5.3, data = data3)
summary(data_glm2)                  
anova(data_glm2)
vip(data_glm2)

data_glm3 <- glm(impulse_buy_score ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness + Q5.3 + income, data = data3)
summary(data_glm3)                  
anova(data_glm3)
vip(data_glm3)

data_glm4 <- glm(impulse_buy_score ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness + Q5.3 + income + Q2.6, data = data3)
summary(data_glm4)                  
anova(data_glm4)
vip(data_glm4)

data_glm5 <- glm(impulse_buy_score ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness + Q5.3 + income + Q2.6 + Q2.4, data = data3)
summary(data_glm5)                  
anova(data_glm5)
vip(data_glm5)

data_glm6 <- glm(impulse_buy_score ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness + Q5.3 + income + Q2.6 + Q2.4 + Q2.2_2, data = data3)
summary(data_glm6)                  
anova(data_glm6)
vip(data_glm6)

data_glm7 <- glm(impulse_buy_score ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness + Q5.3 + income + Q2.6 + Q2.4 + Q2.2_2 + Q2.2_3, data = data3)
summary(data_glm7)                  
anova(data_glm7)
vip(data_glm7)

data_glm8 <- glm(impulse_buy_score ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness + Q5.3 + income + Q2.6 + Q2.4 + Q2.2_2 + Q2.2_3 + Q2.2_4, data = data3)
summary(data_glm8) 
rsq(data_glm8, adj = TRUE)
anova(data_glm8)
vip(data_glm8)

data_glm9 <- glm(impulse_buy_score ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness + Q5.3 + income + Q2.6 + Q2.4 + Q2.2_2 + Q2.2_3 + Q2.2_4 + Q2.2_8, data = data3)
summary(data_glm9) 
rsq(data_glm9)
rsq(data_glm9, adj = TRUE)
anova(data_glm9)
vip(data_glm9)+labs(title="Variable Importance for Model 10")+theme_pubr()+theme(plot.title = element_text(hjust = 0.5))

data_glm10 <- glm(impulse_buy_score ~ extraversion + conscientiousness + Q5.3 + income + Q2.6 + Q2.4 + Q2.2_2 + Q2.2_3 + Q2.2_8, data = data3)
summary(data_glm10)
rsq(data_glm10)
rsq(data_glm10, adj = TRUE)
anova(data_glm10)
vip(data_glm10)+labs(title="Variable Importance for Model 11")+theme_pubr()+theme(plot.title = element_text(hjust = 0.5))

glm_models <- list(data_glm_null, data_glm, data_glm2, data_glm3, data_glm4, data_glm5, data_glm6, data_glm7, data_glm8, data_glm9, data_glm10)
stargazer(glm_models, type = "latex")
#plot linear regression model tests
{
par(mfrow = c(2, 4))
hist(residuals(data_glm9), format = latex)

# Plot residuals vs. fitted values
plot(data_glm9$fitted.values, residuals(data_glm9), xlab = "Fitted values", ylab = "Residuals")

# Plot normal probability plot
qqnorm(residuals(data_glm9))
qqline(residuals(data_glm9))

# Check assumptions of normality and linearity using residual vs predictor variable plots
plot(data3$extraversion, residuals(data_glm9), xlab = "extraversion", ylab = "Residuals")
plot(data3$agreeableness, residuals(data_glm9), xlab = "agreeableness", ylab = "Residuals")
plot(data3$conscientiousness, residuals(data_glm9), xlab = "conscientiousness", ylab = "Residuals")
plot(data3$neg_emotionality, residuals(data_glm9), xlab = "neg_emotionality", ylab = "Residuals")
plot(data3$open_mindedness, residuals(data_glm9), xlab = "open_mindedness", ylab = "Residuals")
}
#export plots
{
jpeg("/Users/andrewmcburney/Desktop/COGS_50.08/linear_regression_tests.jpeg", width = 1200, height = 600)
par(mfrow = c(2, 4))
hist(residuals(data_glm9))

# Plot residuals vs. fitted values
plot(data_glm9$fitted.values, residuals(data_glm9), xlab = "Fitted values", ylab = "Residuals")

# Plot normal probability plot
qqnorm(residuals(data_glm9))
qqline(residuals(data_glm9))

# Check assumptions of normality and linearity using residual vs predictor variable plots
plot(data3$extraversion, residuals(data_glm9), xlab = "extraversion", ylab = "Residuals")
plot(data3$agreeableness, residuals(data_glm9), xlab = "agreeableness", ylab = "Residuals")
plot(data3$conscientiousness, residuals(data_glm9), xlab = "conscientiousness", ylab = "Residuals")
plot(data3$neg_emotionality, residuals(data_glm9), xlab = "neg_emotionality", ylab = "Residuals")
plot(data3$open_mindedness, residuals(data_glm9), xlab = "open_mindedness", ylab = "Residuals")
dev.off()
}
#variance inflation factor multicolinearity test
vif(data_glm9)
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


