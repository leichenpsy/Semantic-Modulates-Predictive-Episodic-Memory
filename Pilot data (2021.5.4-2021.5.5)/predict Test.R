setwd('/Users/leichen/Research project/Predictive memory/Pilot data (2021.5.4-2021.5.5)')
library(readxl)
library(psycho)
library(tidyverse)
library(lmerTest)
library(ggplot2)
library(modelbased)

predictTest <- read_excel('data_organized.xlsx', sheet='predictTest')
predictTest <- data.frame(predictTest)
lmerPredict <- lmer(acc_acount~responseType+(1|sub),predictTest)
anova(lmerPredict)
est_lmerPredict <- estimate_means(lmerPredict)
est_lmerPredict