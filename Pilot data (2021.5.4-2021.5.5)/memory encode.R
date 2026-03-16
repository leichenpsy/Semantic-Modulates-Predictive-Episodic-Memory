setwd('/Users/leichen/Research project/Predictive memory/Pilot data (2021.5.4-2021.5.5)')
library(readxl)
library(psycho)
library(tidyverse)
library(lmerTest)
library(ggplot2)
library(modelbased)

# quarter * type result (model 21: acc; model 22: rt)
memoryEncode1 <- read_excel('data_organized.xlsx', sheet='memoryEncode1')
memoryEncode1 <- data.frame(memoryEncode1)
lm21 <- lmer(acc_ratio~type*quarter+(1|sub),memoryEncode1)
anova(lm21)
estimate21 <- estimate_means(lm21,level = c('type','quarter')) 
estimate21
ggplot(estimate21,aes(x = quarter, y = Mean, color = type, group = type))+
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = CI_low,ymax = CI_high),position = position_dodge(0.3))+
  ylab('acc')+
  xlab('quarter')+
  theme_bw()
lm22 <- lmer(rt~type*quarter+(1|sub),memoryEncode1)
anova(lm22)
estimate22 <- estimate_means(lm22,level = c('type','quarter')) 
ggplot(estimate22,aes(x = quarter, y = Mean, color = type, group = type))+
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = CI_low,ymax = CI_high),position = position_dodge(0.3))+
  ylab('response time')+
  xlab('quarter')+
  theme_bw()

# type * quarter * category result
memoryEncode2 <- read_excel('data_organized.xlsx', sheet='memoryEncode2')
memoryEncode2 <- data.frame(memoryEncode2)
lm23 <- lmer(acc_ratio~type*quarter*category+(1|sub),memoryEncode2)
anova(lm23)
estimate23 <- estimate_means(lm23)
estimate23
