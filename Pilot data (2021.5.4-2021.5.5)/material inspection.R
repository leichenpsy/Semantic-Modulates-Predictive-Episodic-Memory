setwd('/Users/leichen/Research project/Predictive memory/Pilot data (2021.5.4-2021.5.5)')
library(readxl)
library(psycho)
library(tidyverse)
library(lmerTest)
library(ggplot2)
library(modelbased)

memoryEncode5 <- read_excel('data_organized.xlsx', sheet='memoryEncode5')
memoryEncode5 <- data.frame(memoryEncode5)

#subcategory check
lmsub <- lmer(acc_ratio ~ picCategory + (1|sub), memoryEncode5)
anova(lmsub)
est_lmsub <- estimate_means(lmsub,levels = 'picCategory')
est_lmsub
ggplot(est_lmsub,aes(x = picCategory, y = Mean, group = 1),position = position_dodge(0.3))+
  geom_line()+
 
  geom_pointrange(aes(ymin = CI_low,ymax = CI_high), position = position_dodge(0.3))+
  ylab('acc')+
  xlab('subCategory')+
  theme_bw()

#basic category check
lmbasic <- lmer(acc_ratio ~ basicCategory + (1|sub),memoryEncode5)
anova(lmbasic)
est_lmbasic <- estimate_means(lmbasic)
est_lmbasic
ggplot(est_lmbasic, aes(x = basicCategory, y = Mean, group = 1))+
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = CI_low,ymax = CI_high),position = position_dodge(0.3))+
  ylab('acc')+
  xlab('basicCategory')+
  theme_bw()

#categoryA check
caA <- filter(memoryEncode5, basicCategory == 'a')
lmerCaA <- lmer(acc_ratio ~ picCategory + (1|sub),caA)
anova(lmerCaA)
est_lmerCaA <- estimate_means(lmerCaA)
est_lmerCaA
ggplot(est_lmerCaA, aes(x = picCategory, y = Mean, group = 1))+
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = CI_low,ymax = CI_high),position = position_dodge(0.3))+
  ylab('acc')+
  xlab('subCategory')+
  theme_bw()

#categoryB check
caB <- filter(memoryEncode5, basicCategory == 'b')
lmerCaB <- lmer(acc_ratio ~ picCategory + (1|sub),caB)
anova(lmerCaB)
est_lmerCaB <- estimate_means(lmerCaB)
est_lmerCaB
ggplot(est_lmerCaB, aes(x = picCategory, y = Mean, group = 1))+
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = CI_low,ymax = CI_high),position = position_dodge(0.3))+
  ylab('acc')+
  xlab('subCategory')+
  theme_bw()
#categoryC check
caC <- filter(memoryEncode5, basicCategory == 'c')
lmerCaC <- lmer(acc_ratio ~ picCategory + (1|sub),caC)
anova(lmerCaC)
est_lmerCaC <- estimate_means(lmerCaC)
est_lmerCaC
ggplot(est_lmerCaC, aes(x = picCategory, y = Mean, group = 1))+
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = CI_low,ymax = CI_high),position = position_dodge(0.3))+
  ylab('acc')+
  xlab('subCategory')+
  theme_bw()

#categoryD check
caD <- filter(memoryEncode5, basicCategory == 'd')
lmerCaD <- lmer(acc_ratio ~ picCategory + (1|sub),caD)
anova(lmerCaD)
est_lmerCaD <- estimate_means(lmerCaD)
est_lmerCaD
ggplot(est_lmerCaD, aes(x = picCategory, y = Mean, group = 1))+
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = CI_low,ymax = CI_high),position = position_dodge(0.3))+
  ylab('acc')+
  xlab('subCategory')+
  theme_bw()
