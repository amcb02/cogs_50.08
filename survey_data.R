## Andy McBurney
## COGS 50.08: Modeling Mind and Behavior
## Survey Data
library(tidyverse)

data <- (read.csv("/Users/andrewmcburney/Desktop/COGS 50.08/COGS 50.08_February 24, 2023_10.36.csv")[-c(1:2),-c(10:13)])%>%
  replace(.,'Q2.6',ifelse(data$Q2.6 == '', 0, data$Q2.6)) #replace empty observations in Q2.6 with 0



