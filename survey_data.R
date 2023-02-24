## Andy McBurney
## COGS 50.08: Modeling Mind and Behavior
## Survey Data

library(tidyverse)

#read in data
## NOTE: personality trait questions have already been correctly recoded in Qualtrics accoding to 
## https://www.colby.edu/psych/wp-content/uploads/sites/50/2013/08/bfi2s-form.pdf
## DO NOT reverse-key items, it has already been done
data <- (read.csv("/Users/andrewmcburney/Desktop/COGS_50.08/COGS 50.08_February 24, 2023_12.19.csv")[-c(1:2),-c(10:13)])

#replace empty observations in Q2.6 with 0
data <- replace(data, 'Q2.6',ifelse(data$Q2.6 == "", 0, data$Q2.6)) 

#make personality trait questions integers
data[,c(14:61,63:64)] <- sapply(data[,c(14:61, 63:64)],as.integer) 

#create trait variables according to https://www.colby.edu/psych/wp-content/uploads/sites/50/2013/08/bfi2s-form.pdf
data <- data%>%
  group_by(ResponseId)%>%
  mutate(extraversion = mean(c(Q4.1, Q4.6, Q4.11, Q4.16, Q4.21, Q4.26)))%>%
  mutate(agreeableness = mean(c(Q4.2, Q4.7, Q4.12, Q4.17, Q4.22, Q4.27)))%>%
    mutate(conscientiousness = mean(c(Q4.3, Q4.8, Q4.13, Q4.18, Q4.23, Q4.28)))%>%
  mutate(neg_emotionality = mean(c(Q4.4, Q4.9, Q4.14, Q4.19, Q4.24, Q4.24)))%>%
  mutate(open_mindedness = mean(c(Q4.5, Q4.10, Q4.15, Q4.20, Q4.25, Q4.30)))

avg_data <- data%>%
  ungroup()%>%
  summarize(avg_ext = mean(extraversion),
            avg_agr = mean(agreeableness),
            avg_con = mean(conscientiousness),
            avg_neg = mean(neg_emotionality),
            avg_ope = mean(open_mindedness),
            sd_ext = sd(extraversion),
            sd_agr = sd(agreeableness),
            sd_con = sd(conscientiousness),
            sd_neg = sd(neg_emotionality),
            sd_ope = sd(open_mindedness))

#average time to finish survey
avg_time <- as.numeric(mean(as.numeric(data$Duration..in.seconds.))/60)

#correlation matrix of 5 personality traits
cor_matrix <- cor(data[,c(68:72)])

#linear model
data_glm <- glm(Q2.1_1 ~ extraversion + agreeableness + conscientiousness + neg_emotionality + open_mindedness, data = data)

summary(data_glm)                  
