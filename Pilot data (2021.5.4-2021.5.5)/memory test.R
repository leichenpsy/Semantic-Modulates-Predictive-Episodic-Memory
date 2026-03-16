setwd('/Users/leichen/Research project/Predictive memory/Pilot data (2021.5.4-2021.5.5)')
library(readxl)
library(psycho)
library(tidyverse)
library(lmerTest)
library(ggplot2)
library(modelbased)
memoryTestRaw <- read_excel('data_organized.xlsx', sheet='memoryTest_count_loose')

# Compute SDT indices of cue, filler and target images. 
cueIndices <- psycho::dprime(memoryTestRaw$hit_cue,memoryTestRaw$fa_cue,memoryTestRaw$miss_cue,memoryTestRaw$cj_cue)
fillerIndices <- psycho::dprime(memoryTestRaw$hit_filler,memoryTestRaw$fa_filler,memoryTestRaw$miss_filler,memoryTestRaw$cj_filler)
targetIndices <- psycho::dprime(memoryTestRaw$hit_target,memoryTestRaw$fa_target,memoryTestRaw$miss_target,memoryTestRaw$cj_target)

# Combine SDT indices of cue, filler and target into a dataframe
dfcue <- data.frame(cueIndices)
dffiller <- data.frame(fillerIndices)
dftarget <- data.frame(targetIndices)
df <- cbind(dfcue,'sub'= 1:9,'type'= rep('cue',9))
dffiller <- cbind(dffiller,'sub'= 1:9,'type'= rep('filler',9))
dftarget <- cbind(dftarget,'sub'= 1:9,'type'= rep('target',9))
df <- rbind(df,dffiller,dftarget)

# Run mixed-effect linear model and anova test of aprime and dprime of above test.
m1 <- lmer(aprime ~ type + (1|sub),df)
anova(m1)
plotm1 <- boxplot(aprime ~ type, data = df, xlab = 'type',ylab = 'aprime')
plotm1
m2 <- lmer(dprime ~ type + (1|sub),df)
anova(m2)
plotm2 <- boxplot(dprime ~ type, data = df, xlab = 'type',ylab = 'dprime')
plotm2

#Compute SDT indices of type(cue, target)* category(cross, within)
cuecrossindices <- psycho::dprime(memoryTestRaw$hit_cross_cue,memoryTestRaw$fa_cross_cue,memoryTestRaw$miss_cross_cue,memoryTestRaw$cj_cross_cue)
targetcrossindices <- psycho::dprime(memoryTestRaw$hit_cross_target,memoryTestRaw$fa_cross_target,memoryTestRaw$miss_cross_target,memoryTestRaw$cj_cross_target)
cuewithinindices <- psycho::dprime(memoryTestRaw$hit_within_cue,memoryTestRaw$fa_within_cue,memoryTestRaw$miss_within_cue,memoryTestRaw$cj_within_cue)
targetwithinindices <- psycho::dprime(memoryTestRaw$hit_within_target,memoryTestRaw$fa_within_target,memoryTestRaw$miss_within_target,memoryTestRaw$cj_within_target)

# Combine SDT indices of type* category into a dataframe
tcdf <- rbind(data.frame(cuewithinindices),data.frame(cuecrossindices),data.frame(targetcrossindices),data.frame(targetwithinindices))
tcdf <- cbind(tcdf,'sub' = rep(1:9,4))
tcdf <- cbind(tcdf, 'type' = rep(c('cue','target'),each = 18))
tcdf <- cbind(tcdf,'category' = rep(c('within','cross','cross','within'),each = 9))

# Run mixed-effect linear model and anova test of aprime and dprime of above test.
lm3 <- lmer(aprime~type*category+(1|sub),tcdf)
anova(lm3)
plotlm3<-boxplot(aprime ~ type*category, data = tcdf)
plotlm3
lm4 <- lmer(dprime ~ type*category + (1|sub),tcdf)
anova(lm4)
plotlm4 <- boxplot(dprime ~ type*category,data = tcdf)
plotlm4

# Combine a dataframe include filler, flat type*category
flatdf <- tcdf[,1:6]
flatdf <- rbind(flatdf,dffiller[,1:6])
flatdf <-cbind(flatdf, 'condition' = rep(c('cuewithin','cuecross','targetcross','targetwithin','filler'),each = 9))

# Run mixed-effect linear model and anova test of aprime and dprime of above test.
lm5 <- lmer(aprime~condition+(1|sub),flatdf)
anova(lm5)
plotlm5 <-boxplot(aprime~condition,data = flatdf)
plotlm5
lm6 <- lmer(dprime~condition +(1|sub),flatdf)
anova(lm6)
plotlm6<-boxplot(dprime~condition,data = flatdf)
plotlm6

# Compute hit rates, false alarm rates and accuracy level
#Compute hitrates all, each type, each quarter, each type* quarter and each type * category, each type*quarter*category

#Compute hitrates all, each type
hitrates <- data.frame('hitrates_all' = memoryTestRaw$hit_all/(memoryTestRaw$hit_all + memoryTestRaw$miss_all))
hitrates <- cbind(hitrates,data.frame('hitrates_cue' = memoryTestRaw$hit_cue/(memoryTestRaw$hit_cue + memoryTestRaw$miss_cue)))
hitrates <-cbind(hitrates,data.frame('hitrates_target' = memoryTestRaw$hit_target/(memoryTestRaw$hit_target + memoryTestRaw$miss_target)))
hitrates <- cbind(hitrates,data.frame('hitrates_filler' = memoryTestRaw$hit_filler/(memoryTestRaw$hit_filler + memoryTestRaw$miss_filler)))

#Compute hitrates each quarter
hitrates <- cbind(hitrates,'hitrates_q1' = memoryTestRaw$hit_all_q1/(memoryTestRaw$hit_all_q1 + memoryTestRaw$miss_all_q1))
hitrates <- cbind(hitrates,'hitrates_q2' = memoryTestRaw$hit_all_q2/(memoryTestRaw$hit_all_q2 + memoryTestRaw$miss_all_q2))
hitrates <- cbind(hitrates,'hitrates_q3' = memoryTestRaw$hit_all_q3/(memoryTestRaw$hit_all_q3 + memoryTestRaw$miss_all_q3))
hitrates <- cbind(hitrates,'hitrates_q4' = memoryTestRaw$hit_all_q4/(memoryTestRaw$hit_all_q4 + memoryTestRaw$miss_all_q4))

#Compute hitrates type * quarter
hitrates <- cbind(hitrates, 'hitrates_cue_q1' = memoryTestRaw$hit_cue_q1/(memoryTestRaw$hit_cue_q1 + memoryTestRaw$miss_cue_q1))
hitrates <- cbind(hitrates, 'hitrates_cue_q2' = memoryTestRaw$hit_cue_q2/(memoryTestRaw$hit_cue_q2 + memoryTestRaw$miss_cue_q2))
hitrates <- cbind(hitrates, 'hitrates_cue_q3' = memoryTestRaw$hit_cue_q3/(memoryTestRaw$hit_cue_q3 + memoryTestRaw$miss_cue_q3))
hitrates <- cbind(hitrates, 'hitrates_cue_q4' = memoryTestRaw$hit_cue_q4/(memoryTestRaw$hit_cue_q4 + memoryTestRaw$miss_cue_q4))
hitrates <- cbind(hitrates, 'hitrates_filler_q1' = memoryTestRaw$hit_filler_q1/(memoryTestRaw$hit_filler_q1 + memoryTestRaw$miss_filler_q1))
hitrates <- cbind(hitrates, 'hitrates_filler_q2' = memoryTestRaw$hit_filler_q2/(memoryTestRaw$hit_filler_q2 + memoryTestRaw$miss_filler_q2))
hitrates <- cbind(hitrates, 'hitrates_filler_q3' = memoryTestRaw$hit_filler_q3/(memoryTestRaw$hit_filler_q3 + memoryTestRaw$miss_filler_q3))
hitrates <- cbind(hitrates, 'hitrates_filler_q4' = memoryTestRaw$hit_filler_q4/(memoryTestRaw$hit_filler_q4 + memoryTestRaw$miss_filler_q4))
hitrates <- cbind(hitrates, 'hitrates_target_q1' = memoryTestRaw$hit_target_q1/(memoryTestRaw$hit_target_q1 + memoryTestRaw$miss_target_q1))
hitrates <- cbind(hitrates, 'hitrates_target_q2' = memoryTestRaw$hit_target_q2/(memoryTestRaw$hit_target_q2 + memoryTestRaw$miss_target_q2))
hitrates <- cbind(hitrates, 'hitrates_target_q3' = memoryTestRaw$hit_target_q3/(memoryTestRaw$hit_target_q3 + memoryTestRaw$miss_target_q3))
hitrates <- cbind(hitrates, 'hitrates_target_q4' = memoryTestRaw$hit_target_q4/(memoryTestRaw$hit_target_q4 + memoryTestRaw$miss_target_q4))

#Compute hitrates type * category
hitrates <- cbind(hitrates, 'hitrates_cue_within'= memoryTestRaw$hit_within_cue/(memoryTestRaw$hit_within_cue + memoryTestRaw$miss_within_cue))
hitrates <- cbind(hitrates, 'hitrates_cue_cross' = memoryTestRaw$hit_cross_cue/(memoryTestRaw$hit_cross_cue + memoryTestRaw$miss_cross_cue))
hitrates <- cbind(hitrates, 'hitrates_target_within'= memoryTestRaw$hit_within_target/(memoryTestRaw$hit_within_target + memoryTestRaw$miss_within_target))
hitrates <- cbind(hitrates, 'hitrates_target_cross' = memoryTestRaw$hit_cross_target/(memoryTestRaw$hit_cross_target + memoryTestRaw$miss_cross_target))

#Compute hitrates quarter * category
hitrates <- cbind(hitrates, 'hitrates_within_q1' = memoryTestRaw$hit_within_all_q1/(memoryTestRaw$hit_within_all_q1 + memoryTestRaw$miss_within_all_q1),'hitrates_within_q2' = memoryTestRaw$hit_within_all_q2/(memoryTestRaw$hit_within_all_q2 + memoryTestRaw$miss_within_all_q2),'hitrates_within_q3' = memoryTestRaw$hit_within_all_q3/(memoryTestRaw$hit_within_all_q3 + memoryTestRaw$miss_within_all_q3),'hitrates_within_q4' = memoryTestRaw$hit_within_all_q4/(memoryTestRaw$hit_within_all_q4 + memoryTestRaw$miss_within_all_q4),'hitrates_cross_q1' = memoryTestRaw$hit_cross_all_q1/(memoryTestRaw$hit_cross_all_q1 + memoryTestRaw$miss_cross_all_q1),'hitrates_cross_q2' = memoryTestRaw$hit_cross_all_q2/(memoryTestRaw$hit_cross_all_q2 + memoryTestRaw$miss_cross_all_q2),'hitrates_cross_q3' = memoryTestRaw$hit_cross_all_q3/(memoryTestRaw$hit_cross_all_q3 + memoryTestRaw$miss_cross_all_q3),'hitrates_cross_q4' = memoryTestRaw$hit_cross_all_q4/(memoryTestRaw$hit_cross_all_q4 + memoryTestRaw$miss_cross_all_q4))

#Compute hitrates type * category * quarter
hitrates <- cbind(hitrates, 'hitrates_cue_within_q1' = memoryTestRaw$hit_within_cue_q1/(memoryTestRaw$hit_within_cue_q1 + memoryTestRaw$miss_within_cue_q1))
hitrates <- cbind(hitrates, 'hitrates_cue_within_q2' = memoryTestRaw$hit_within_cue_q2/(memoryTestRaw$hit_within_cue_q2 + memoryTestRaw$miss_within_cue_q2))
hitrates <- cbind(hitrates, 'hitrates_cue_within_q3' = memoryTestRaw$hit_within_cue_q3/(memoryTestRaw$hit_within_cue_q3 + memoryTestRaw$miss_within_cue_q3))
hitrates <- cbind(hitrates, 'hitrates_cue_within_q4' = memoryTestRaw$hit_within_cue_q4/(memoryTestRaw$hit_within_cue_q4 + memoryTestRaw$miss_within_cue_q4))

hitrates <- cbind(hitrates, 'hitrates_cue_cross_q1' = memoryTestRaw$hit_cross_cue_q1/(memoryTestRaw$hit_cross_cue_q1 + memoryTestRaw$miss_cross_cue_q1))
hitrates <- cbind(hitrates, 'hitrates_cue_cross_q2' = memoryTestRaw$hit_cross_cue_q2/(memoryTestRaw$hit_cross_cue_q2 + memoryTestRaw$miss_cross_cue_q2))
hitrates <- cbind(hitrates, 'hitrates_cue_cross_q3' = memoryTestRaw$hit_cross_cue_q3/(memoryTestRaw$hit_cross_cue_q3 + memoryTestRaw$miss_cross_cue_q3))
hitrates <- cbind(hitrates, 'hitrates_cue_cross_q4' = memoryTestRaw$hit_cross_cue_q4/(memoryTestRaw$hit_cross_cue_q4 + memoryTestRaw$miss_cross_cue_q4))

hitrates <- cbind(hitrates, 'hitrates_target_within_q1' = memoryTestRaw$hit_within_target_q1/(memoryTestRaw$hit_within_target_q1 + memoryTestRaw$miss_within_target_q1))
hitrates <- cbind(hitrates, 'hitrates_target_within_q2' = memoryTestRaw$hit_within_target_q2/(memoryTestRaw$hit_within_target_q2 + memoryTestRaw$miss_within_target_q2))
hitrates <- cbind(hitrates, 'hitrates_target_within_q3' = memoryTestRaw$hit_within_target_q3/(memoryTestRaw$hit_within_target_q3 + memoryTestRaw$miss_within_target_q3))
hitrates <- cbind(hitrates, 'hitrates_target_within_q4' = memoryTestRaw$hit_within_target_q4/(memoryTestRaw$hit_within_target_q4 + memoryTestRaw$miss_within_target_q4))

hitrates <- cbind(hitrates, 'hitrates_target_cross_q1' = memoryTestRaw$hit_cross_target_q1/(memoryTestRaw$hit_cross_target_q1 + memoryTestRaw$miss_cross_target_q1))
hitrates <- cbind(hitrates, 'hitrates_target_cross_q2' = memoryTestRaw$hit_cross_target_q2/(memoryTestRaw$hit_cross_target_q2 + memoryTestRaw$miss_cross_target_q2))
hitrates <- cbind(hitrates, 'hitrates_target_cross_q3' = memoryTestRaw$hit_cross_target_q3/(memoryTestRaw$hit_cross_target_q3 + memoryTestRaw$miss_cross_target_q3))
hitrates <- cbind(hitrates, 'hitrates_target_cross_q4' = memoryTestRaw$hit_cross_target_q4/(memoryTestRaw$hit_cross_target_q4 + memoryTestRaw$miss_cross_target_q4))

#Combine dataframe type effect
typehrdf <- data.frame('hitrate' = hitrates[,2 ])
typehrdf <- rbind(typehrdf,data.frame('hitrate' = hitrates[,3]))
typehrdf <- rbind(typehrdf,data.frame('hitrate' = hitrates[,4]))
typehrdf <- cbind(typehrdf,'sub' = rep(1:9,3))
typehrdf <- cbind(typehrdf, 'type' = rep(c('cue','target','filler'),each = 9))

# run mixed-effect linear model and anova with above dataset
lm7 <- lmer(hitrate~type+(1|sub),typehrdf)
anova(lm7)
plotlm7 <- boxplot(hitrate~type, data = typehrdf)
plotlm7

# Combine dataframe quarter effect
quarterhrdf <- data.frame('hitrate' = hitrates$hitrates_q1)
quarterhrdf <- rbind(quarterhrdf,data.frame('hitrate' = c(hitrates$hitrates_q2,hitrates$hitrates_q3,hitrates$hitrates_q4)))
quarterhrdf<-cbind(quarterhrdf,'sub'=rep(1:9,4),'quarter'= rep(c('q1','q2','q3','q4'),each = 9))


# run mixed-effect linear model and anova with above dataset
lm8 <- lmer(hitrate~quarter + (1|sub),quarterhrdf)
anova(lm8)
plotlm8 <- boxplot(hitrate~quarter,data = quarterhrdf)
plotlm8

#Combine dataframe type * category effect
tycahrdf <- data.frame('hitrate' = hitrates$hitrates_cue_cross)
tycahrdf <- rbind(tycahrdf, data.frame('hitrate' = c(hitrates$hitrates_cue_within,hitrates$hitrates_target_cross, hitrates$hitrates_target_within)))
tycahrdf <- cbind(tycahrdf,'sub' = rep(1:9,4),'type'= rep(c('cue','target'),each = 18),'category'= rep(c('cross','within'),each = 9))

# run mixed-effect linear model and anova with above dataset
lm9 <- lmer(hitrate~type*category+(1|sub),tycahrdf)
anova(lm9)
plotlm9 <- boxplot(hitrate~type*category, data = tycahrdf)
plotlm9

#flat dataframe to compare type*category vs. filler
flattycahrdf <- data.frame('hitrate' = tycahrdf$hitrate)
flattycahrdf <- rbind(flattycahrdf,data.frame('hitrate' = typehrdf[19:27,1]))
flattycahrdf <- cbind(flattycahrdf,'sub' = rep(1:9,5),'condition' = rep(c('cuecross','cuewithin','targetcross','targetwithin','filler'),each = 9))
flattycahrdf

# run mixed-effect linear model and anova with above dataset
lm10 <- lmer(hitrate~condition + (1|sub),flattycahrdf)
anova(lm10)
plotlm10 <- boxplot(hitrate~condition, data = flattycahrdf)
plotlm10

#Combine dataframe for quarter * type effect
qtypedf <- data.frame('hitrate' =hitrates$hitrates_cue_q1)
qtypedf <- rbind(qtypedf, data.frame('hitrate' = hitrates$hitrates_cue_q2),data.frame('hitrate' = hitrates$hitrates_cue_q3),data.frame('hitrate' = hitrates$hitrates_cue_q4),data.frame('hitrate' = hitrates$hitrates_target_q1),data.frame('hitrate' = hitrates$hitrates_target_q2),data.frame('hitrate' = hitrates$hitrates_target_q3),data.frame('hitrate' = hitrates$hitrates_target_q4),data.frame('hitrate' = hitrates$hitrates_filler_q1),data.frame('hitrate' = hitrates$hitrates_filler_q2),data.frame('hitrate' = hitrates$hitrates_filler_q3),data.frame('hitrate' = hitrates$hitrates_filler_q4))
qtypedf <- cbind(qtypedf,'sub' = rep(1:9,12),'type' = rep(c('cue','target','filler'),each = 36),'quarter' = rep(rep(c('q1','q2','q3','q4'),each = 9),3))

# run mixed-effect linear model and anova with above dataset
lm11 <- lmer(hitrate~type*quarter+(1|sub),qtypedf)
anova(lm11)
plotlm11 <- boxplot(hitrate ~ type*quarter, data = qtypedf)
plotlm11

#Combine dataframe for category * quarter effect
qcahrdf <- data.frame('hitrate' = hitrates$hitrates_within_q1)
qcahrdf <- rbind(qcahrdf, data.frame('hitrate' = hitrates$hitrates_within_q2),data.frame('hitrate' = hitrates$hitrates_within_q3),data.frame('hitrate' = hitrates$hitrates_within_q4),data.frame('hitrate' = hitrates$hitrates_cross_q1),data.frame('hitrate' = hitrates$hitrates_cross_q2),data.frame('hitrate' = hitrates$hitrates_cross_q3),data.frame('hitrate' = hitrates$hitrates_cross_q4))
qcahrdf <- cbind(qcahrdf,'sub' = rep(1:9,8),'quarter' = rep(rep(c('q1','q2','q3','q4'),each = 9),2),'category' = rep(c('within','cross'),each = 36))

# run mixed-effect linear model and anova with above dataset
lm12 <- lmer(hitrate~quarter*category + (1|sub),qcahrdf)
anova(lm12)
plotlm12 <- boxplot(hitrate~quarter*category,data = qcahrdf)
plotlm12

#Combine dataframe for (category+filler) * quarter
fqcahrdf <- qcahrdf
filler<- qtypedf[73:108,1:2]
fqcahrdf [73:108,1:2] = filler
fqcahrdf[73:108,3:4] = list(rep(c('q1','q2','q3','q4'),each = 9),rep('filler',36))

#run linear mixed-effect model and anova with above dataset
lm13 <- lmer(hitrate~quarter*category +(1|sub),fqcahrdf)
anova(lm13)
plotlm13 <- boxplot(hitrate~quarter * category, data = fqcahrdf)
plotlm13

#Combine dataframe for category * type * quarter effect
tcaqhrdf <- data.frame('hitrate'= hitrates$hitrates_cue_cross_q1)
tcaqhrdf <- rbind(tcaqhrdf,data.frame('hitrate' = hitrates$hitrates_cue_cross_q2),data.frame('hitrate' = hitrates$hitrates_cue_cross_q3),data.frame('hitrate' = hitrates$hitrates_cue_cross_q4),data.frame('hitrate' = hitrates$hitrates_cue_within_q1),data.frame('hitrate' = hitrates$hitrates_cue_within_q2),data.frame('hitrate' = hitrates$hitrates_cue_within_q3),data.frame('hitrate' = hitrates$hitrates_cue_within_q4),data.frame('hitrate' = hitrates$hitrates_target_cross_q1),data.frame('hitrate' = hitrates$hitrates_target_cross_q2),data.frame('hitrate' = hitrates$hitrates_target_cross_q3),data.frame('hitrate' = hitrates$hitrates_target_cross_q4),data.frame('hitrate' = hitrates$hitrates_target_within_q1),data.frame('hitrate' = hitrates$hitrates_target_within_q2),data.frame('hitrate' = hitrates$hitrates_target_within_q3),data.frame('hitrate' = hitrates$hitrates_target_within_q4))
tcaqhrdf <- cbind(tcaqhrdf,'sub' = rep(1:9,16),'type' = rep(c('cue','target'),each = 72), 'category' = rep(rep(c('cross','within'),each = 36),2),'quarter' = rep(rep(c('q1','q2','q3','q4'),each = 9),4))

#run linear mixed-effect model and anova with above dataset
lm14 <- lmer(hitrate~type*quarter*category + (1|sub),tcaqhrdf)
anova(lm14)
plotlm14 <- boxplot(hitrate~type*quarter*category, data = tcaqhrdf)
plotlm14

#Run interaction analysis for the above test
estimated_means <- estimate_means(lm14,levels = c('category','quarter'))
estimated_means
ggplot(estimated_means,aes(x = quarter, y = Mean, color = category, group = category))+
         geom_line(position = position_dodge(0.3)) +
         geom_pointrange(aes(ymin = CI_low,ymax = CI_high),position = position_dodge(0.3))+
  ylab('hitrate')+
  xlab('quarter')+
  theme_bw()

es <- estimate_means(lm14)
ggplot(es,aes(x = quarter, y = Mean, color = category, group = category),position = position_dodge(0.3))+
  geom_line()+
  facet_grid(.~type)+
  geom_pointrange(aes(ymin = CI_low,ymax = CI_high), position = position_dodge(0.3))+
  ylab('hitrate')+
  xlab('quarter')+
  theme_bw()

est <- estimate_means(lm14,levels = c('type','quarter'))
est
ggplot(est,aes(x = quarter, y = Mean, color = type, group = type))+
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = CI_low,ymax = CI_high),position = position_dodge(0.3))+
  ylab('hitrate')+
  xlab('quarter')+
  theme_bw()
  


