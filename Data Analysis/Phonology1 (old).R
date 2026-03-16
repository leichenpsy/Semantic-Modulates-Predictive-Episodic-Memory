library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(psycho)
library(psych)
library(ggsignif)
library(emmeans)
library(ggpubr)
library(pwr)
library(WebPower)
library(effectsize)

## Compile data files
setwd('/Users/leichen/Research project/Predictive memory/Phonologyv1 Data (2022.03.24 - 2022.04.07)')
n_subject <- length(list.files('/Users/leichen/Research project/Predictive memory/Phonologyv1 Data (2022.03.24 - 2022.04.07)'))
data_all <- list.files(path = '/Users/leichen/Research project/Predictive memory/Phonologyv1 Data (2022.03.24 - 2022.04.07)') %>%
  lapply(read.csv) %>%
  bind_rows()

## Define the variables
coln_trans <- c('correctResponse','currSession','currTask','currTrial','pairCaType', 'pairSiType','picType','predictPair','predictType','response','responseTime','round','sessionNum','sizeType','stimulus','type','scrambleType','sessionStartTime','sessionEndTime','subject','expStartTime','expEndTime')

########## Subset Encoding Session Data #############
encodeData <- data_all %>%
  filter(currTask == 'encodePictures')
encodeData <- encodeData[,coln_trans] %>%
  mutate(acc = ifelse(correctResponse == response,1,0)) %>%
  mutate(category = substr(stimulus,1,1)) %>%
  mutate(subCategory = substr(stimulus,1,2)) 


##aggregate acc data for each subcategory
accEncode1 <- encodeData %>%
  group_by(subject,subCategory) %>%
  summarize(meanAccSubCategory = mean(acc))
summary(accEncode1)
coverTask1 <- lmer(meanAccSubCategory~subCategory + (1|subject), data = accEncode1)
summary(coverTask1) 
anova(coverTask1)
ggplot(accEncode1, aes(x = subCategory, y = meanAccSubCategory)) +stat_summary(fun = 'mean',geom = 'bar')

##aggregate acc data for each category
accEncode2 <- encodeData %>%
  group_by(subject, category)%>%
  summarize(meanAccCategory = mean(acc))
coverTask2 <- lmer(meanAccCategory ~ category + (1|subject), data = accEncode2)
summary(coverTask2)
anova(coverTask2)
ggplot(accEncode2, aes(x = category, y = meanAccCategory)) + stat_summary(fun = 'mean', geom = 'bar')

##aggregate acc data across all category
accEncode3 <- encodeData %>%
  group_by(subject) %>%
  summarize(meanAcc = mean(acc))
summary(accEncode3$meanAcc)
describe(accEncode3$meanAcc)
ggplot(accEncode3,aes(x = meanAcc)) + geom_histogram(binwidth = 0.1)
table(accEncode3$meanAcc) 

##proportion of 'no response' 
accEncode4 <- encodeData %>%
  mutate(noResponse = ifelse(response == 'no response detected',1,0)) %>%
  group_by(subject) %>%
  summarize(noResponse = mean(noResponse))
Q <- quantile(accEncode3$meanAcc)

##merge acc and 'no response'
encodeScammerCheck <- merge(accEncode3,accEncode4,by = 'subject')
ggplot(encodeScammerCheck,aes(x = noResponse, y = meanAcc)) + geom_point()

############# Subset math data ###########
mathData <- data_all %>%
  filter(currTask == 'mathDistract') %>%
  mutate(acc = ifelse(correctResponse == response,1,0)) %>%
  group_by(subject) %>%
  summarise(meanMathAcc = mean(acc))

########### Subset Memory Test Data ############
memoryData <- data_all[, coln_trans] %>%
  filter(currTask == 'memoryTest') %>%
  mutate(acc = case_when(correctResponse == 'k' & response %in% c('j','k') ~ 1, correctResponse == 'k' & !response %in% c('j','k') ~ 0, correctResponse == 'd' & response %in% c('d','f') ~ 1, correctResponse == 'd' & !response %in% c('d','f') ~ 0))

## Add quartile to both encode data and memory test data
quartile <- encodeData[,c('subject','currTrial','stimulus')] %>%
  group_by('subject') %>%
  arrange('currTrial') %>%
  cbind(q = rep(1:4, times = n_subject, each = 64), half = rep(1:2, times = n_subject, each = 128))
encodeData <- merge(encodeData, quartile[,c('subject','q','stimulus', 'half')],by = c('subject','stimulus'))
memoryData <- merge(memoryData,quartile[,c('subject','q','stimulus', 'half')],by = c('subject','stimulus'),all = TRUE)

#mark memory response as hit, miss, fa, cr
memoryTest <- memoryData %>%
  filter(response != 'no response detected') %>% #filter out no response trials
  mutate(sdt = case_when(picType == 'foil' & acc == 1 ~ 'cr', picType == 'foil' & acc == 0 ~ 'fa', picType == 'old' & acc == 1 ~ 'hit', picType == 'old' & acc == 0 ~ 'miss') )
  
## summary of memory test results
memoryAcc <- memoryTest %>%
  group_by(subject) %>%
  summarise(meanAcc = mean(acc))
memoryResult <- memoryTest %>%
  group_by(subject, sdt) %>%
  summarise(meanAcc = n()/256)
summary(memoryResult)

memoryCount <- memoryTest %>%
  group_by(subject, type, predictType) %>%
  count(sdt) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n')
memoryCount[,4:7][is.na(memoryCount[,4:7])]<-0

## calculate precision
memoryPrecision <- memoryCount %>%
  mutate(precision = n_hit/(n_hit + n_fa)) 
  
memoryPrecisionModel1 <- lmer(precision ~ type + (1|subject), data = memoryPrecision)
anova(memoryPrecisionModel1)
summary(memoryPrecisionModel1)
memoryPrecisionSEM <- memoryPrecision %>%
  group_by(type, predictType) %>%
  summarize(meanPrecision = mean(precision), se = sd(precision)/sqrt(length(precision)))

ggboxplot(memoryPrecision, x = 'predictType', y = 'precision', color = 'type', add = 'jitter')
ggbarplot(memoryPrecision, x = 'predictType', y = 'precision', fill = 'type', add = 'mean_se', position = position_dodge(0.8)) #+ geom_jitter(aes(x = type, y = precision, fill = 'predictType' ), color = 'darkgrey', alpha = 0.65, position = position_jitterdodge(jitter.height = .1, jitter.width = .2))

#calculate sensitivity/hit_rate
memorySensitivity <- memoryCount %>%
  mutate(sensitivity = n_hit/(n_hit + n_miss)) 
  
memoryHitType <- memorySensitivity %>%
  group_by(type, predictType) %>%
  summarize(hitRateMean = mean(sensitivity), se = sd(sensitivity)/sqrt(length(sensitivity)))

memoryHitRateModel1 <- lmer(sensitivity ~ type + (1|subject), data = memorySensitivity)
anova(memoryHitRateModel1)
summary(memoryHitRateModel1)

memorySensitivityCom <- memorySensitivity %>%
  filter(type != 'filler')
memoryHitRateModel2 <- lmer(sensitivity ~ type * predictType + (1|subject), data = memorySensitivityCom)
anova(memoryHitRateModel2)
summary(memoryHitRateModel2)
emHit <- emmeans(memoryHitRateModel2, ~type * predictType)
contrast(emHit, 'pairwise', by = 'predictType')
contrast(emHit, 'pairwise', by = 'type')
hitFlat <- memorySensitivity %>%
  pivot_wider(id_cols = 'subject', names_from = c('type','predictType'), values_from = 'sensitivity')
t.test(hitFlat$cue_within, hitFlat$filler_NA, paired = T)
t.test(hitFlat$cue_cross, hitFlat$filler_NA, paired = T)
t.test(hitFlat$target_within, hitFlat$filler_NA, paired = T)
t.test(hitFlat$target_cross, hitFlat$filler_NA, paired = T)

ggplot(memorySensitivity, aes(x = type, y = sensitivity, color = predictType)) + geom_boxplot()
ggboxplot(memorySensitivity, x = 'predictType', y = 'sensitivity', fill = 'type') + geom_jitter(aes(x = predictType, y = sensitivity, fill = type), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1))
p <- ggbarplot(memorySensitivity, x = 'predictType', y = 'sensitivity', fill = 'type', add = 'mean_se', position = position_dodge(0.8))# + geom_jitter(aes(x = type, y = sensitivity, fill = predictType), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1))
ggpar(p, ylim = c(0,0.5)) + scale_y_continuous(breaks = get_breaks(n = 6))

## Calculate hit_rate + q
memoryHitQ <- memoryData %>%
  filter(response != 'no response detected') %>% #filter out no response trials
  mutate(sdt = case_when(picType == 'foil' & acc == 1 ~ 'cr', picType == 'foil' & acc == 0 ~ 'fa', picType == 'old' & acc == 1 ~ 'hit', picType == 'old' & acc == 0 ~ 'miss') ) %>%
  group_by(subject, q, type, predictType) %>%
  count(sdt) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n')
memoryHitQ[,5:8][is.na(memoryHitQ[,5:8])]<-0 
memoryHitQ <- memoryHitQ %>%
  filter(!is.na(q)) %>%
  mutate(hit_rate = n_hit/(n_hit + n_miss))
memoryHitQModel1 <- lmer(hit_rate ~ q * type * predictType + (1|subject), data = memoryHitQ)
anova(memoryHitQModel1)
ggbarplot(memoryHitQ, x = 'predictType', y = 'hit_rate', fill = 'type', add = 'mean_se', position = position_dodge(0.8), facet.by = 'q', panel.labs = list(q = c ('1st quartile', '2nd quartile', '3rd quartile', '4th quartile')))
p <- ggline(memoryHitQ, x = 'q', y = 'hit_rate', color = 'predictType',facet.by = 'type', add = 'mean_se', position = position_dodge(0.2))
ggpar(p, ylim = c(0,0.6)) + scale_y_continuous(breaks = get_breaks(n = 6))

## Calculate hit_rate + half
memoryHitH <- memoryData %>%
  filter(response != 'no response detected') %>% #filter out no response trials
  mutate(sdt = case_when(picType == 'foil' & acc == 1 ~ 'cr', picType == 'foil' & acc == 0 ~ 'fa', picType == 'old' & acc == 1 ~ 'hit', picType == 'old' & acc == 0 ~ 'miss') ) %>%
  group_by(subject, half, type, predictType) %>%
  count(sdt) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n')
memoryHitH[,5:8][is.na(memoryHitH[,5:8])]<-0 
memoryHitH <- memoryHitH %>%
  filter(!is.na(half)) %>%
  mutate(hit_rate = n_hit/(n_hit + n_miss))
memoryHitHModel1 <- lmer(hit_rate ~ half * type * predictType + (1|subject), data = memoryHitH)
anova(memoryHitHModel1)
p <- ggbarplot(memoryHitH, x = 'predictType', y = 'hit_rate', fill = 'type', add = 'mean_se', position = position_dodge(0.8), facet.by = 'half', panel.labs = list(half = c ('1st half', '2nd half'))) + geom_hline(yintercept = 0.5, linetype="dashed", color = "black")
ggpar(p, ylim = c(0,0.6)) + scale_y_continuous(breaks = get_breaks(n = 6))

## calculate confident hit 
memoryCofHit <- data_all[, coln_trans] %>%
  filter(currTask == 'memoryTest') %>%
  mutate(acc = ifelse(response == correctResponse, 1,0)) %>%
  group_by(subject, picType, type, predictType) %>%
  summarise(meanAcc = mean(acc)) %>%
  filter(picType == 'old')
ggbarplot(memoryCofHit, x = 'predictType', y = 'meanAcc', fill = 'type', add = 'mean_se', position = position_dodge(0.8))

memoryCofHitQ <- data_all[, coln_trans] %>%
  filter(currTask == 'memoryTest') %>%
  mutate(acc = ifelse(response == correctResponse, 1,0)) 
memoryCofHitQ <- merge(memoryCofHitQ,quartile[,c('subject','q','stimulus', 'half')],by = c('subject','stimulus'),all = TRUE)
memoryCofHitQ <- memoryCofHitQ %>%
  group_by(subject, picType, type, predictType,q) %>%
  summarise(meanAcc = mean(acc)) %>%
  filter(picType == 'old')
ggbarplot(memoryCofHitQ, x = 'predictType', y = 'meanAcc', fill = 'type', add = 'mean_se', position = position_dodge(0.8), facet.by = 'q')


#calculate dprime
indices <- psycho::dprime(memoryCount$n_hit,memoryCount$n_fa,memoryCount$n_miss,memoryCount$n_cr)
dprimeMemory <- cbind(memoryCount, indices) 
ggplot(dprimeMemory, aes(x = type, y = dprime, fill= predictType)) + geom_boxplot()
ggplot(dprimeMemory, aes(x = type, y = aprime, color = predictType)) + geom_boxplot()
ggboxplot(dprimeMemory, x = 'predictType', y = 'dprime', fill = 'type') + geom_jitter(aes(x = predictType, y = dprime, fill = type), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1))

ggboxplot(dprimeMemory, x = 'predictType', y = 'aprime', fill = 'type') + geom_jitter(aes(x = predictType, y = aprime, fill = type), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1))

p <- ggbarplot(dprimeMemory, x = 'predictType', y = 'dprime', fill = 'type', add = 'mean_se', position = position_dodge(0.8))# + geom_jitter(aes(x = type, y = dprime, fill = predictType), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1))
ggpar(p, ylim = c(0,0.8)) + scale_y_continuous(breaks = get_breaks(n = 6))

ggbarplot(dprimeMemory, x = 'predictType', y = 'aprime', fill = 'type', add = 'mean_se', position = position_dodge(0.8))# + geom_jitter(aes(x = type, y = dprime, fill = predictType), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1))


aprimeModel1 <- lmer(aprime ~ type + (1|subject), data = dprimeMemory)
anova(aprimeModel1)
summary(aprimeModel1)
aprimeModel2 <- lmer(aprime ~ type * predictType + (1|subject), data = dprimeMemory)
anova(aprimeModel2)

dprimeModel1 <- lmer(dprime ~ type + (1|subject), data = dprimeMemory)
anova(dprimeModel1)
dprimeModel2 <- lmer(dprime ~ type * predictType + (1|subject), data = dprimeMemory)
anova(dprimeModel2)
emdprime <- emmeans(dprimeModel2, ~ type * predictType)
contrast(emdprime, 'pairwise', by = 'predictType')
dprimeFlat <- dprimeMemory %>%
  pivot_wider(id_cols = c('subject'), names_from = c('type','predictType'), values_from = 'dprime')
t.test(dprimeFlat$cue_within, dprimeFlat$filler_NA, paired = T)


#Calculate false alarm rate
memoryFaRate <- memoryCount %>%
  mutate(fa_rate = n_fa/(n_cr + n_fa)) 
ggplot(memoryFaRate, aes(x = type, y = fa_rate, color = predictType)) + geom_boxplot()
ggboxplot(memoryFaRate, x = 'predictType', y = 'fa_rate', fill = 'type') + geom_jitter(aes(x = predictType, y = fa_rate, fill = type), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1))
p <- ggbarplot(memoryFaRate, x = 'predictType', y = 'fa_rate', fill = 'type', add = 'mean_se', position = position_dodge(0.8))# + geom_jitter(aes(x = type, y = fa_rate, fill = predictType), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1))
ggpar(p, ylim = c(0,0.4)) + scale_y_continuous(breaks = get_breaks(n = 6))

memoryFaRateModel1 <- lmer(fa_rate ~ type + (1|subject), data = memoryFaRate)
anova(memoryFaRateModel1)
summary(memoryFaRateModel1)
memoryFaRateModel2 <- lmer(fa_rate ~ type * predictType + (1|subject), data = memoryFaRate)
anova(memoryFaRateModel2)
emFa <- emmeans(memoryFaRateModel2, ~type * predictType)
contrast(emFa,'pairwise',by = 'predictType')
FaFlat <- memoryFaRate %>%
  pivot_wider(id_cols = c('subject'), names_from = c('type','predictType'), values_from = 'fa_rate')
t.test(FaFlat$cue_within, FaFlat$filler_NA, paired = T)
t.test(FaFlat$cue_cross, FaFlat$filler_NA, paired = T)
t.test(FaFlat$target_cross, FaFlat$filler_NA, paired = T)
t.test(FaFlat$target_within, FaFlat$filler_NA, paired = T)

## Calculate Confident False Alarm 
memoryCofFa <- data_all[, coln_trans] %>%
  filter(currTask == 'memoryTest' & picType == 'foil') %>%
  mutate(fa = case_when(correctResponse == 'd' & response == 'k' ~1, correctResponse == 'k' & response == 'd' ~1)) %>%
  mutate(fa = ifelse(is.na(fa), 0,1)) %>%
  group_by(subject,predictType, type, picType) %>%
  summarise(meanFa = mean(fa))
ggbarplot(memoryCofFa, x = 'predictType', y = 'meanFa', fill = 'type', add = 'mean_se',position = position_dodge(0.8)) 


## Calculate Confident Correct Rejection
memoryCofCr <- data_all[, coln_trans] %>%
  filter(currTask == 'memoryTest') %>%
  mutate(acc = ifelse(response == correctResponse, 1,0)) %>%
  group_by(subject, picType, type, predictType) %>%
  summarise(meanAcc = mean(acc)) %>%
  filter(picType == 'foil')
ggbarplot(memoryCofCr, x = 'predictType', y = 'meanAcc', fill = 'type', add = 'mean_se',position = position_dodge(0.8))


# Calculate bias
ggplot(dprimeMemory, aes(x = type, y = beta, color = predictType)) + geom_boxplot()
betaModel1 <- lmer(beta ~ type + (1|subject), data = dprimeMemory)
anova(betaModel1)
betaModel2 <- lmer(beta ~ type * predictType + (1|subject), data = dprimeMemory)
anova(betaModel2)

ggplot(dprimeMemory, aes(x= type, y = bppd, color = predictType)) + geom_boxplot()
bppdModel1 <- lmer(bppd ~ type + (1|subject), data = dprimeMemory)
anova(bppdModel1)
bppdModel2 <- lmer(bppd ~ type * predictType + (1|subject), data = dprimeMemory)
anova(bppdModel2)

# Calculate memory test response time
memoryRT <- memoryData %>%
  filter(acc == 1) %>%
  group_by(subject, type, predictType) %>%
  summarise(rt = median(responseTime))
memoryRTModel1 <- lmer(rt ~ type * predictType + (1|subject), data = memoryRT)
anova(memoryRTModel1)
ggboxplot(memoryRT, x = 'predictType', y = 'rt', fill = 'type') + geom_jitter(aes(x = predictType, y = rt, fill = type), color = 'grey', alpha = 0.8, position = position_jitterdodge(jitter.width = 0.2))
ggbarplot(memoryRT, x = 'predictType', y = 'rt', fill = 'type', position = position_dodge(0.8), add = 'mean_se')

memoryHitRT <- memoryData %>%
  filter(picType == 'old' & acc == 1) %>%
  group_by(subject, type, predictType) %>%
  summarise(rt = median(responseTime))
memoryHitRTModel1 <- lmer(rt ~ type * predictType + (1|subject), data = memoryHitRT)
anova(memoryHitRTModel1)
ggboxplot(memoryHitRT, x = 'predictType', y = 'rt', fill = 'type') + geom_jitter(aes(x = predictType, y = rt, fill = type), color = 'grey', alpha = 0.8, position = position_jitterdodge(jitter.width = 0.2))
ggbarplot(memoryHitRT, x = 'predictType', y = 'rt', fill = 'type', position = position_dodge(0.8), add = 'mean_se')

memoryCrRT <- memoryData %>%
  filter(picType == 'foil' & acc == 1) %>%
  group_by(subject, type, predictType) %>%
  summarise(rt = median(responseTime))
memoryCrRTModel1 <- lmer(rt ~ type * predictType + (1|subject), data = memoryCrRT)
anova(memoryCrRTModel1)
ggbarplot(memoryCrRT, x = 'predictType', y = 'rt', fill = 'type', position = position_dodge(0.8), add = 'mean_se')


################## Cover task - Prediction #####################

##Calculate Acc
coverTaskAccType <- encodeData %>%
  group_by(subject, type) %>%
  summarize(acc = mean(acc))
ggplot(coverTaskAccType, aes(x = type, y = acc)) + geom_boxplot()

coverTaskAccTypeModel1 <- lmer(acc ~ type + (1|subject), data = coverTaskAccType)
anova(coverTaskAccTypeModel1)

coverTaskAccPredictType <- encodeData %>%
  group_by(subject, type, predictType) %>%
  summarize(acc = mean(acc))
  

ggboxplot(coverTaskAccPredictType, x = 'predictType', y = 'acc', fill = 'type') + geom_jitter(aes(x = predictType, y = acc, fill = type), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2))

p <- ggbarplot(coverTaskAccPredictType, x = 'type', y = 'acc', fill = 'predictType', add = 'mean_se', position = position_dodge(0.8))#+ geom_jitter(aes(x = predictType, y = acc, fill = type), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1))
ggpar(p, ylim = c(0,1)) + scale_y_continuous(breaks = get_breaks(n = 6))


coverTaskAccPredictTypeModel1 <- lmer(acc~type * predictType + (1|subject), data = coverTaskAccPredictType)
anova(coverTaskAccPredictTypeModel1)
emCoverTaskAcc <- emmeans(coverTaskAccPredictTypeModel1, ~type * predictType)
contrast(emCoverTaskAcc, 'pairwise', by = 'predictType')
contrast(emCoverTaskAcc, 'pairwise', by = 'type')

coverTaskAccPredictTypeFlat <- coverTaskAccPredictType %>%
  pivot_wider(names_from = c('type', 'predictType'), values_from = 'acc')
t.test(coverTaskAccPredictTypeFlat$filler_NA, coverTaskAccPredictTypeFlat$cue_cross, paired = TRUE)
t.test(coverTaskAccPredictTypeFlat$filler_NA, coverTaskAccPredictTypeFlat$cue_within, paired = TRUE)
t.test(coverTaskAccPredictTypeFlat$filler_NA, coverTaskAccPredictTypeFlat$target_cross, paired = TRUE)
t.test(coverTaskAccPredictTypeFlat$filler_NA, coverTaskAccPredictTypeFlat$target_within, paired = TRUE)

## Calculate ACC + Half
coverTaskAccH <- encodeData %>%
  group_by(subject, type, predictType, half) %>%
  summarise(acc = mean(acc))
coverTaskAccHModel1 <- lmer(acc ~ type * predictType * half + (1|subject), data = coverTaskAccH)
anova(coverTaskAccHModel1)
ggboxplot(coverTaskAccH, x = 'predictType', y = 'acc', fill = 'type') + geom_jitter(aes(x = predictType, y = acc, fill = type), position = position_jitterdodge(jitter.width = 0.2), color = 'grey', alpha = 0.8) + facet_wrap(~half)
p <- ggbarplot(coverTaskAccH, x = 'predictType', y = 'acc', color = 'type', add = 'mean_se', position = position_dodge(0.8), facet.by = 'half', panel.labs = list(half = c('1st half', '2nd half')))
ggpar(p, ylim = c(0,1)) + scale_y_continuous(breaks = get_breaks(n = 6))

emcoverTaskAccH <- emmeans(coverTaskAccHModel1, ~half*type*predictType)
contrast(emcoverTaskAccH, 'pairwise', by = 'half')




##Calculate median RT
coverTaskRTType <- encodeData %>%
  filter(response == correctResponse)%>%
  group_by(subject, type) %>%
  mutate(rt = as.numeric(responseTime)) %>%
  summarize(rt = median(rt)) 
ggplot(coverTaskRTType, aes(x = type, y = rt, group = type)) + geom_boxplot()

coverTaskRTPredictType <- encodeData %>%
  filter(response == correctResponse)%>%
  group_by(subject, type, predictType) %>%
  mutate(rt = as.numeric(responseTime)) %>%
  summarize(rt = median(rt))
describe(coverTaskRTPredictType$rt)
coverTaskRTModel1 <- lmer(rt ~ type * predictType + (1|subject), data = coverTaskRTPredictType)
anova(coverTaskRTModel1)
ggboxplot(coverTaskRTPredictType,x = 'predictType', y = 'rt', fill = 'type') + geom_jitter(aes(x = predictType, y = rt, fill = type), color = 'grey',alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1)) 
p <- ggbarplot(coverTaskRTPredictType, x = 'predictType', y = 'rt', fill = 'type', add = 'mean_se', position = position_dodge(0.8)) #+ geom_jitter(aes(x = predictType, y = rt, fill = type), color = 'grey', alpha = 0.9, position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1))
ggpar(p, ylim = c(0,800)) + scale_y_continuous(breaks = get_breaks(n = 8))
emcoverTaskRT <- emmeans(coverTaskRTModel1, ~ type * predictType)
contrast(emcoverTaskRT, 'pairwise', by = 'predictType')

coverTaskRTTypeQ <- encodeData %>%
  filter(response == correctResponse)%>%
  group_by(subject, type, q) %>%
  mutate(rt = as.numeric(responseTime)) %>%
  summarise(rt = median(rt))
coverTaskRTTypeQPlot <- coverTaskRTTypeQ %>%
  group_by(type, q) %>%
  summarise(rt = mean(rt))
ggplot(coverTaskRTTypeQPlot, aes(x = q, y = rt, color = type, group = type)) + geom_point() + geom_line()

coverTaskRTPredictTypeQ <- encodeData %>%
  filter(response == correctResponse) %>%
  mutate(rt = as.numeric(responseTime), q = factor(q, levels = c(1,2,3,4))) %>%
  group_by(subject, type, predictType, q) %>%
  summarise(rt = median(rt))
ggboxplot(coverTaskRTPredictTypeQ, x = 'predictType', y = 'rt', fill = 'type') + geom_jitter(aes(x = predictType, y = rt, fill = type), position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1), alpha = 0.8) + facet_wrap(~q)
ggbarplot(coverTaskRTPredictTypeQ, x = 'predictType', y = 'rt', fill = 'type', color = 'type', position = position_dodge(0.8)) + geom_jitter(aes(x = predictType, y = rt, fill = type), position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1), alpha = 0.8) + facet_wrap(~q)

coverTaskRTPredictTypeQPlot <- coverTaskRTPredictTypeQ %>%
  group_by(subject, predictType, type, q) %>%
  summarise(rt = median(rt))%>%
  pivot_wider(names_from = c(type, predictType), values_from = rt) %>%
  pivot_longer(cols = c(cue_within, cue_cross, target_within, target_cross, filler_NA), names_to = 'combo', values_to = 'rt') 

ggplot(coverTaskRTPredictTypeQPlot, aes(x = q, y = rt, color = combo, group = combo)) + geom_point() + geom_line()
ggbarplot(coverTaskRTPredictTypeQ, x = 'type', y = 'rt', fill = 'predictType', add = 'mean_se', position = position_dodge(0.8), facet.by = 'q')
ggline(coverTaskRTPredictTypeQPlot, x = 'q', y = 'rt', color = 'combo', add = 'mean_se', position = position_dodge(0.1)) 



## Calculate RT + Half
coverTaskRTH <- encodeData %>%
  filter(response == correctResponse)%>%
  group_by(subject, type, predictType, half) %>%
  mutate(rt = as.numeric(responseTime)) %>%
  summarise(rt = median(rt))
coverTaskRTHModel <- lmer(rt ~ type * predictType*half + (1|subject), data = coverTaskRTH)
anova(coverTaskRTHModel)

p <- ggbarplot(coverTaskRTH, x = 'predictType', y = 'rt', color = 'type', facet.by = 'half', panel.labs = list(half = c('1st half', '2nd half')), add = 'mean_se', position = position_dodge(0.8))
ggpar(p, ylim = c(0,800)) + scale_y_continuous(breaks = get_breaks(n = 8))
emcoverTaskRTH <- emmeans(coverTaskRTHModel, ~ type * predictType * half)
contrast(emcoverTaskRTH,'pairwise', by = 'half')

coverTaskRTHLine <- coverTaskRTH %>%
  pivot_wider(names_from = c(type, predictType), values_from = rt) %>%
  pivot_longer(cols = c(cue_within, cue_cross, target_within, target_cross, filler_NA), names_to = 'combo', values_to = 'rt') 

ggline(coverTaskRTHLine, x = 'half', y = 'rt', color = 'combo', add = 'mean_se', position = position_dodge(0.2))
  

############ calculate SLP by log transformation ############
logTrans <- encodeData %>%
  mutate(rt = case_when(acc == 1 ~ as.numeric(responseTime),acc == 0 ~ 0),trialNo = as.numeric(currTrial), logTime = log10(rt))
diff <- logTrans %>%
  group_by(subject) %>%
  arrange(trialNo, .by_group = TRUE) %>%
  mutate(diff = logTime - lead(logTime, default = NA))

##filter out NA trials##
SLP <- diff %>%
  mutate(diffType = case_when(type == 'cue' ~ 'cue-target', type == 'target' & lead(type) == 'filler' ~ 'target-filler', type == 'target' & lead(type) == 'cue' ~ 'target-cue', type == 'filler' & lead(type) == 'filler' ~ 'filler-filler', type == 'filler' & lead(type) == 'cue' ~ 'filler-cue')) %>%
  mutate(diffCa = case_when(category == lead(category) ~ 'withinCa', category != lead(category) ~ 'acrossCa'))%>%
  mutate(fillerCa = case_when(subCategory == lead(subCategory) ~ 'same', subCategory != lead(subCategory) ~ 'diff')) %>%
  mutate(diffType = ifelse(diffType == 'filler-filler' & fillerCa == 'same', 'filler-filler(same)', diffType))%>%
  filter(!is.na(diff) & diff != -Inf & diff != Inf)

meanSLP <- SLP %>%
  group_by(subject, half,diffType, diffCa) %>%
  summarise(meanSLP = mean(diff)) %>%
  filter(diffType == 'cue-target' || diffType == 'filler-filler' || diffType == 'filler-filler(same)')
ggboxplot(meanSLP,x = 'diffType', y = 'meanSLP', color = 'diffCa', add = 'jitter') 
p <- ggbarplot(meanSLP, x = 'diffType', y = 'meanSLP', fill = 'diffCa', add = 'mean_se',position= position_dodge(0.8))
ggpar(p, ylim = c(0,0.1)) + scale_y_continuous(breaks = get_breaks(n = 6))
meanSLPModel<- lmer(meanSLP~diffType * diffCa + (1|subject), data = meanSLP)
anova(meanSLPModel)
acrossPair <- meanSLP %>%
  filter(diffType == 'cue-target' & diffCa == 'acrossCa')
t.test(acrossPair$meanSLP, mu = 0)
withinPair <- meanSLP %>%
  filter(diffType == 'cue-target' & diffCa == 'withinCa')
t.test(withinPair$meanSLP, mu = 0)




p <- ggbarplot(meanSLP, x = 'diffType', y = 'meanSLP', fill = 'diffCa', add = 'mean_se',position= position_dodge(0.8), facet.by = 'half', panel.labs = list(half = c('1st half', '2nd half'))) + rotate_x_text(45) #+ geom_jitter(aes(x = diffType, y = meanSLP, fill = 'diffCa'), color = 'darkgrey', alpha = 0.65, position = position_jitterdodge(jitter.height = .1, jitter.width = .2))
ggpar(p, ylim = c(0,0.1)) + scale_y_continuous(breaks = get_breaks(n = 6))
acrossPairH <- meanSLP %>%
  filter(diffType == 'cue-target' & diffCa == 'acrossCa' & half == 2)
t.test(acrossPairH$meanSLP, mu = 0)


meanSLPQ <- SLP %>%
  group_by(subject, q, diffType, diffCa) %>%
  summarise(meanSLP = mean(diff))
ggbarplot(meanSLPQ, x = 'diffType', y = 'meanSLP', fill = 'diffCa', add = 'mean_se',position= position_dodge(0.8), facet.by = 'q', panel.labs = list(q = c('1st quartile', '2nd quartile', '3rd quartile', '4th quartile'))) + rotate_x_text(45)





################## Prediction test ########################
#Subset prediction data
predictData <- data_all[,coln_trans] %>%
  filter(currTask == 'predictTest' & correctResponse != 'no correct response' & response != 'no response detected') %>%
  mutate(acc = ifelse(response == correctResponse,1,0),
         pairCaType = ifelse(substr(predictPair,1,1) == substr(predictPair,3,3),'within','across')) 

predictResult <- predictData %>%
  group_by(subject, scrambleType, pairCaType) %>%
  summarise(meanAcc = mean(acc))
ggplot(predictResult, aes(x = pairCaType, y = meanAcc, color = scrambleType)) + geom_boxplot()
ggboxplot(predictResult, x = 'scrambleType', y = 'meanAcc', color = 'pairCaType', add = 'jitter')


## Make bar plot with jitter ##
predictResultBar <- predictResult %>%
  ungroup() %>%
  group_by(scrambleType, pairCaType) %>%
  summarise(
    n = n(),
    mean = mean(meanAcc),
    sd = sd(meanAcc),
    se = sd/sqrt(n),
    ci = qt(0.975, df = n - 1) * sd / sqrt(n))
ggplot(predictResultBar, aes(x = scrambleType, y = mean, fill = pairCaType)) + geom_bar(stat = 'identity', position = 'dodge') + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1, position=position_dodge(.9), color = 'black') + theme_classic() + geom_hline(yintercept = 0.5, linetype="dashed", color = "black") 
p <- ggbarplot(predictResult, x = 'scrambleType', y = 'meanAcc', fill = 'pairCaType', add = c('mean_se'), position= position_dodge(0.8))+ geom_hline(yintercept = 0.5, linetype="dashed", color = "black")
ggpar(p, ylim = c(0,0.8)) + scale_y_continuous(breaks = get_breaks(n = 8))
ggboxplot(predictResult, x = 'scrambleType', y = 'meanAcc', fill = 'pairCaType') + geom_jitter(aes(x = scrambleType, y = meanAcc, fill = pairCaType), color = 'grey', position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.1), alpha = 0.8)


predictTest <- predictResult %>%
  filter(scrambleType != 'swapping')
predictTestModel <- lmer(meanAcc ~ pairCaType * scrambleType + (1|subject), data = predictTest)
anova(predictTestModel)
summary(predictTestModel)

predictTestChance <- predictResult %>%
  filter(scrambleType == 'firstSame'& pairCaType == 'across')
t.test(predictTestChance$meanAcc, mu = 0.5)

# Calculate prediction RT
predictRT <- predictData %>%
  filter(acc == 1) %>%
  group_by(subject, scrambleType, pairCaType) %>%
  summarise(rt = median(responseTime))
predictRTModel1 <- lmer(rt ~ pairCaType * scrambleType + (1|subject), data = predictRT)
anova(predictRTModel1)
ggboxplot(predictRT, x = 'scrambleType', y = 'rt', fill = 'pairCaType') + geom_jitter(aes(x = scrambleType, y = rt, fill = pairCaType), color = 'grey', position = position_jitterdodge(jitter.width = 0.2), alpha = 0.8)
ggbarplot(predictRT, x = 'scrambleType', y = 'rt', fill = 'pairCaType', add = c('mean_se'), position= position_dodge(0.8))


################# Calculate Power and sample size ################
sherman1aMemory <- read.table('Experiment1a_MemoryTest.txt', header = F)
colnames(sherman1aMemory) <- list('subj','condition','picType','acc','sdt', 'confidence', 'fileName','category')
sherman1aMemory <- sherman1aMemory %>%
  filter(picType == 1) %>%
  group_by(subj, condition, picType) %>%
  summarise(meanAcc = mean(as.numeric(acc)))
anoM <- lmer(meanAcc ~ condition + (1|subj), data = sherman1aMemory)
cohens_f(anoM)
### Calculate Power : Sherman F main effect of condition 
wp.rmanova(n = 30L, ng = 1, nm = 3, f = 0.22, nscor = 1,
           alpha = 0.05,type = 1) ## power = 0.1671372
sherman1attest <- sherman1aMemory %>%
  filter(condition != 3)
cohens_d(meanAcc ~ condition, paired = T, data = sherman1attest)
# a vs x, d = -0.42, a vs b, d = -0.51
### Calculate Power :  Sherman t test 
wp.t(n1 = 30, d = -0.42, type = 'paired') ## Power: 0.6040701
wp.t(n1 = 30, d = -0.51, type = 'paired') ## Power: 0.7703204

### Calculate cohen's f false alarm rate main effect of Type
cohens_f(memoryFaRateModel1) ## cohen's_f : 0.23

## Calculate cohen's f false alarm for Type * predictType
memoryFaRateF <- memoryFaRate %>%
  filter(type != 'filler')
memoryFaRateFModel <- lmer(fa_rate ~ type * predictType + (1|subject), data = memoryFaRateF)
cohens_f(memoryFaRateFModel) ## type: 0.22, predictType: 0.1, type*predictType: 0.01
anova(memoryFaRateFModel)
## Calculate cohen'd for false alaram t.test
memoryFaRatettest <- memoryFaRate %>%
  filter(type != 'cue' & (predictType == 'cross' || type == 'filler'))

cohens_d(fa_rate ~ type, paired = T, data = memoryFaRatettest) ## within_target vs filler = 0.58; cross_target vs filler = 0.25

## Calculate cohen's d dprime t.test
dprimeMemoryttest <- dprimeMemory %>%
  filter(type != 'target' & (predictType == 'within' || type == 'filler'))
cohens_d(dprime ~ type, paired = T, data = dprimeMemoryttest) ## cross_target vs filler = 0.04; within_cue vs filler = 0.14

## Calculate cohen's f  dprime for Type * predictType
dprimeMemoryF <- dprimeMemory %>%
  filter(type != 'filler')
dprimeMemoryFModel <- lmer(dprime~type * predictType + (1|subject), data = dprimeMemoryF)
cohens_f(dprimeMemoryFModel) ## type * predictType: 0.11


## Calculate cohen's d predict Test t.test


# across_swapping
predictTestAcrossS <- predictResult %>%
  filter(pairCaType == 'across' & scrambleType == 'swapping')
cohens_d(predictTestAcrossS$meanAcc, mu = 0.5) ## d = 0.16
wp.t(n1 = 25, d = 0.16, type = 'one.sample') ## power = 0.1198935

# across_firstSame
predictTestAcross1 <- predictResult %>%
  filter(pairCaType == 'across' & scrambleType == 'firstSame')
cohens_d(predictTestAcross1$meanAcc, mu = 0.5) ## d = 0.27
wp.t(n1 = 25, d = 0.27, type = 'one.sample') ## power = 0.2540131

# within_firstSame
predictTestWithin1 <- predictResult %>%
  filter(pairCaType == 'within' & scrambleType == 'firstSame')
cohens_d(predictTestWithin1$meanAcc, mu = 0.5) ## d = 0.28
wp.t(n1 = 25, d = 0.28, type = 'one.sample') ## power = 0.2695222


##### Calculate sample size based on false alarm

## false alarm main effect of Type : n = 183
wp.rmanova(power = 0.8, ng = 1, nm = 3, f = 0.23, nscor = 1,
           alpha = 0.05,type = 1)

## false alarm F test Type * predictType. Type main effect: n = 226
wp.rmanova(power = 0.8, ng = 2, nm = 4, f = 0.22, nscor = 1,
           alpha = 0.05,type = 1)

## false alarm F test Type * predictType. predictType main effect: n = 1091
wp.rmanova(power = 0.8, ng = 2, nm = 4, f = 0.1, nscor = 1,
           alpha = 0.05,type = 1)

## false alarm t test. within_target vs filler: n = 25.31977
wp.t(power = 0.8, d = -0.58, type = 'paired')

## false alarm t test. cross_target vs filler: n = 127.52
wp.t(power = 0.8, d = -0.25, type = 'paired')


##### Calculate sample size based on dprime

## dprime t test. within_cue vs filler : n = 402.377
wp.t(power = 0.8, d = -0.14, type = 'paired')

## dprime F test Type * predictType Type * predictType interaction: n = 902.3428
wp.rmanova(power = 0.8, ng = 2, nm = 4, f = 0.11, nscor = 1,
           alpha = 0.05,type = 2)


##### Calculate sample size based on predict test

## across_swapping vs 0.5
wp.t(alpha = 0.05, power = 0.8, d = 0.16, type = 'one.sample') #n = 308.5224
wp.t(alpha = 0.05, power = 0.8, d = 0.27, type = 'one.sample') #n = 109
t.test(predictTestAcrossS$meanAcc, mu = 0.5)


##################### Add semantic similarity score ##################
correlationMatrix = data.frame(correlationMatrix)
coverTaskData <- encodeData %>%
  filter(type != 'filler') %>%
  mutate(currTrial = as.integer(currTrial)) %>%
  arrange(subject, currTrial) 
  
### Add semantic Similarity to cover task data ###

for(par in 1: length(unique(coverTaskData$subject))){
 for (trial in 1: 128){
   rowIndex <- 128 *(par-1) + trial
   if(coverTaskData[rowIndex,'type'] == 'cue'){
     coverTaskData[rowIndex,'semanticSimilarity'] = correlationMatrix[coverTaskData[rowIndex,'subCategory'], coverTaskData[rowIndex + 1,'subCategory']]
     coverTaskData[rowIndex, 'pair'] = paste(coverTaskData[rowIndex,'subCategory'], coverTaskData[rowIndex+1, 'subCategory'], sep = '')
   }
   else{
     coverTaskData[rowIndex,'semanticSimilarity'] = correlationMatrix[coverTaskData[rowIndex-1,'subCategory'], coverTaskData[rowIndex,'subCategory']]
     coverTaskData[rowIndex, 'pair'] = paste(coverTaskData[rowIndex-1,'subCategory'], coverTaskData[rowIndex, 'subCategory'], sep = '')
   }
 }
}


### Add semantic similarity to SLP data ###
subCategoryData <- coverTaskData[,c('subject', 'subCategory', 'semanticSimilarity', 'pair','type')]
subCategoryData <- subCategoryData %>%
  filter(type == 'cue')%>%
  arrange(subject, subCategory)
SLPData <- diff %>%
  mutate(diffType = case_when(type == 'cue' ~ 'cue-target', type == 'target' & lead(type) == 'filler' ~ 'target-filler', type == 'target' & lead(type) == 'cue' ~ 'target-cue', type == 'filler' & lead(type) == 'filler' ~ 'filler-filler', type == 'filler' & lead(type) == 'cue' ~ 'filler-cue')) %>%
  filter(diffType == 'cue-target') %>%
  arrange(subject, subCategory) %>%
  cbind(subCategoryData[,c('semanticSimilarity','pair')]) %>%
  filter(!is.na(diff) & diff != -Inf & diff != Inf)

### Add semantic similarity to memory test data ###

memoryTestData <- memoryData %>%
  mutate(subCategory = substr(stimulus, 1,2))
subCategoryData <- coverTaskData[,c('subject', 'subCategory', 'semanticSimilarity', 'pair')]
subCategoryData <- subCategoryData %>%
  rbind(subCategoryData) %>%
  arrange(subject, subCategory)
memoryTestData <- memoryTestData %>%
  filter(type != 'filler') %>%
  arrange(subject, subCategory) %>%
  cbind(subCategoryData[,c('semanticSimilarity','pair')])

### Add semantic similarity to predict test data ###

subCategoryData <- subCategoryData %>% 
  distinct()%>%
  arrange(subject, pair) %>%
  slice(rep(1:n(), each = 6))
predictTestData <- predictData %>%
  arrange(subject, predictPair) %>%
  cbind(subCategoryData[,'semanticSimilarity'])
colnames(predictTestData)[length(colnames(predictTestData))] <- 'semanticSimilarity'


############### semantic similarity score as predictor ###########

#### Covertask + semantic Similarity ###

semanticCoverTaskAcc <- coverTaskData %>%
  group_by(subject,type, pair) %>%
  summarise(meanAcc = mean(acc), semanticSimilarity = mean(semanticSimilarity))
seCoverTaskModel <- lmer(meanAcc ~ type * semanticSimilarity + (1|subject), data = semanticCoverTaskAcc)
anova(seCoverTaskModel)
summary(seCoverTaskModel)
ggscatter(semanticCoverTaskAcc,x = 'semanticSimilarity', y = 'meanAcc', color = 'type', add = "reg.line", conf.int = T, cor.coef = T)

semanticCoverTaskAccQ <- coverTaskData %>%
  group_by(subject,type,q, pair) %>%
  summarise(meanAcc = mean(acc), semanticSimilarity = mean(semanticSimilarity))
seCoverTaskQModel <- lmer(meanAcc ~ q * type * semanticSimilarity + (1|subject), data = semanticCoverTaskAccQ)
anova(seCoverTaskQModel)
summary(seCoverTaskQModel)
ggscatter(semanticCoverTaskAccQ,x = 'semanticSimilarity', y = 'meanAcc', color = 'type', add = "reg.line", conf.int = T, cor.coef = T, facet.by = 'q')

semanticCoverTaskRT <- coverTaskData %>%
  filter(acc == 1) %>%
  group_by(subject,type, pair) %>%
  summarise(rt = median(responseTime), semanticSimilarity = mean(semanticSimilarity))
seCoverTaskRTModel <- lmer(rt ~ type * semanticSimilarity + (1|subject), data = semanticCoverTaskRT)  
anova(seCoverTaskRTModel)
summary(seCoverTaskRTModel)
ggscatter(semanticCoverTaskRT, x = 'semanticSimilarity', y = 'rt', color = 'type', add = 'reg.line', conf.int = T, cor.coef = T)

semanticCoverTaskRTQ <- coverTaskData %>%
  filter(acc == 1) %>%
  group_by(subject,type,q, pair) %>%
  summarise(rt = median(responseTime), semanticSimilarity = mean(semanticSimilarity))
seCoverTaskRTQModel <- lmer(rt ~ type * q * semanticSimilarity + (1|subject), data = semanticCoverTaskRTQ)
anova(seCoverTaskRTQModel)
summary(seCoverTaskRTQModel)
ggscatter(semanticCoverTaskRTQ, x = 'semanticSimilarity', y = 'rt', color = 'type', add = 'reg.line', conf.int = T, cor.coef = T, facet.by = 'q')

### Memory test + semantic similarity ####

## Mark miss, hit, cr & fa
memoryTestCount <- memoryTestData %>%
  filter(response != 'no response detected') %>% #filter out no response trials
  mutate(sdt = case_when(picType == 'foil' & acc == 1 ~ 'cr', picType == 'foil' & acc == 0 ~ 'fa', picType == 'old' & acc == 1 ~ 'hit', picType == 'old' & acc == 0 ~ 'miss') ) %>%
  group_by(subject,type,sdt, pair) %>%
  summarise(semanticSimilarity = mean(semanticSimilarity), n = n()) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n')
memoryTestCount[,5:8][is.na(memoryTestCount[,5:8])]<-0

memoryTestCountQ <- memoryTestData %>%
  filter(response != 'no response detected') %>% #filter out no response trials
  mutate(sdt = case_when(picType == 'foil' & acc == 1 ~ 'cr', picType == 'foil' & acc == 0 ~ 'fa', picType == 'old' & acc == 1 ~ 'hit', picType == 'old' & acc == 0 ~ 'miss') ) %>%
  group_by(subject,type,sdt,q, pair) %>%
  summarise(semanticSimilarity = mean(semanticSimilarity), n = n()) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n')
memoryTestCountQ[,6:9][is.na(memoryTestCountQ[,6:9])] <- 0
  

## Hit_rate
memoryTestHit <- memoryTestCount %>%
  mutate(hit_rate = n_hit/(n_hit + n_miss)) 
memoryTestHitModel <- lmer(hit_rate ~ type * semanticSimilarity + (1|subject), data = memoryTestHit)
anova(memoryTestHitModel)
summary(memoryTestHitModel)
ggscatter(memoryTestHit, x = 'semanticSimilarity', y = 'hit_rate', color = 'type', add = 'reg.line', conf.int = T, cor.coef = T)

memoryTestHitQ <- memoryTestCountQ %>%
  mutate(hit_rate = n_hit/(n_hit + n_miss)) %>%
  filter(q <= 4)
memoryTestHitQModel <- lmer(hit_rate ~ type * q * semanticSimilarity + (1|subject), data = memoryTestHitQ)
anova(memoryTestHitQModel)
summary(memoryTestHitQModel)
ggscatter(memoryTestHitQ, x = 'semanticSimilarity', y = 'hit_rate', color = 'type', add = 'reg.line', conf.int = T, cor.coef = T, facet.by = 'q')

## False_alarm
memoryTestFa <- memoryTestCount %>%
  mutate(fa_rate = n_fa/(n_cr + n_fa)) 
memoryTestFaModel <- lmer(fa_rate ~ type * semanticSimilarity + (1|subject), data = memoryTestFa)
anova(memoryTestFaModel)
summary(memoryTestFaModel) 
ggscatter(memoryTestFa, x = 'semanticSimilarity', y = 'fa_rate', color = 'type', add = 'reg.line', conf.int = T, cor.coef = T)


## dprime
indicesTest <- psycho::dprime(memoryTestCount$n_hit,memoryTestCount$n_fa,memoryTestCount$n_miss,memoryTestCount$n_cr)
dprimeMemoryTest <- cbind(memoryTestCount, indicesTest) 
dprimeMemoryTestModel <- lmer(dprime ~ type * semanticSimilarity + (1|subject), data = dprimeMemoryTest)
anova(dprimeMemoryTestModel)
summary(dprimeMemoryTestModel)
ggscatter(dprimeMemoryTest, x = 'semanticSimilarity', y = 'dprime', color = 'type', add = 'reg.line', conf.int = T, cor.coef = T)

## Order test + semanticSimilarity
predictTestSe <- predictTestData %>%
  group_by(subject, scrambleType, predictPair) %>%
  summarise(semanticSimilarity = mean(semanticSimilarity), meanAcc = mean(acc))
predictTestSeModel <- lmer(meanAcc ~ semanticSimilarity * scrambleType + (1|subject), data = predictTestSe)
anova(predictTestSeModel)
summary(predictTestSeModel)
ggscatter(predictTestSe, x = 'semanticSimilarity', y = 'meanAcc',  add = 'reg.line', conf.int = T, cor.coef = T, facet.by = 'scrambleType', color = 'blue')

## SLP + semanticSimilarity
SLPDataSe <- SLPData %>%
  group_by(subject, pair) %>%
  summarise(meanSLP = mean(diff), semanticSimilarity = mean(semanticSimilarity))
SLPDataSeModel <- lmer(meanSLP ~ semanticSimilarity + (1|subject), data = SLPDataSe)
anova(SLPDataSeModel)
summary(SLPDataSeModel)
ggscatter(SLPDataSe, x = 'semanticSimilarity', y = 'meanSLP', add = 'reg.line', conf.int = T, cor.coef = T)


############ Add SLP to memory test data ############

SLPIndex <- SLPData %>%
  group_by(subject, pair) %>%
  summarise(meanSLP = mean(diff)) %>%
  arrange(subject,pair)
SLPMemory <- SLPIndex %>%
  slice(rep(1:n(), each = 64))
memoryTestSLP <- memoryTestData %>%
  arrange(subject, pair) %>%
  cbind(SLPMemory[, c('meanSLP')])

### count hit, miss, cr and fa ###
memoryTestSLPCount <- memoryTestSLP %>%
  filter(response != 'no response detected') %>% #filter out no response trials
  mutate(sdt = case_when(picType == 'foil' & acc == 1 ~ 'cr', picType == 'foil' & acc == 0 ~ 'fa', picType == 'old' & acc == 1 ~ 'hit', picType == 'old' & acc == 0 ~ 'miss') ) %>%
  group_by(subject,type, pair,sdt, predictType) %>%
  summarise(meanSLP = mean(meanSLP), n = n()) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n')
memoryTestSLPCount[,6:9][is.na(memoryTestSLPCount[,6:9])]<-0

## SLP + Hit
memorySLPHit <- memoryTestSLPCount %>%
  mutate(hit_rate = n_hit/(n_hit + n_miss))
memorySLPHitModel <- lmer(hit_rate ~ type *predictType * meanSLP + (1|subject), data = memorySLPHit)
anova(memorySLPHitModel)
summary(memorySLPHitModel)
ggscatter(memorySLPHit, x = 'meanSLP', y = 'hit_rate', group = 'predictType', color = 'predictType', add = 'reg.line', conf.int = T, cor.coef = T, facet.by = 'type')

## SLP + False Alarm
memorySLPFa <- memoryTestSLPCount %>%
  mutate(fa_rate = n_fa/(n_cr + n_fa))
memorySLPFaModel <- lmer(fa_rate ~ type * predictType * meanSLP + (1|subject), data = memorySLPFa)
anova(memorySLPFaModel)
summary(memorySLPFaModel)
ggscatter(memorySLPFa, x = 'meanSLP', y = 'fa_rate', group = 'predictType', color = 'predictType', add = 'reg.line', conf.int = T, cor.coef = T, facet.by = 'type')

## SLP + dprime
indicesSLP <- psycho::dprime(memoryTestSLPCount$n_hit,memoryTestSLPCount$n_fa,memoryTestSLPCount$n_miss,memoryTestSLPCount$n_cr)
dprimeMemorySLPTest <- cbind(memoryTestSLPCount, indicesSLP) 
dprimeMemorySLPTestModel <- lmer(dprime ~ type *predictType * meanSLP + (1|subject), data = dprimeMemorySLPTest)
anova(dprimeMemorySLPTestModel)
summary(dprimeMemorySLPTestModel)
ggscatter(dprimeMemorySLPTest, x = 'meanSLP', y = 'dprime', color = 'predictType', add = 'reg.line', conf.int = T, cor.coef = T, facet.by = 'type')


############## Add SLP to Predict Test Data #################

SLPPredictTest <- SLPIndex %>%
  arrange(subject, pair) %>%
  slice(rep(1:n(), each = 12))
predictSLP <- predictData %>%
  arrange(subject, predictPair) %>%
  cbind(SLPPredictTest[, 'meanSLP']) %>%
  group_by(subject, predictPair, scrambleType, pairCaType) %>%
  summarise(meanAcc = mean(acc), meanSLP = mean(meanSLP))
predictSLPModel <- lmer(meanAcc ~ scrambleType * pairCaType * meanSLP + (1|subject), data = predictSLP)
anova(predictSLPModel)
summary(predictSLPModel)
ggscatter(predictSLP, x = 'meanSLP', y = 'meanAcc', color = 'pairCaType', add = 'reg.line',group = 'pairCaType', conf.int = T, cor.coef = T, facet.by = 'scrambleType')

############## Add predict test score to memory test data ###############

predictMatch <- predictData %>%
  group_by(subject, predictPair, scrambleType) %>%
  summarise(meanAcc = mean(acc)) %>%
  arrange(subject, predictPair) %>%
  pivot_wider(names_from = 'scrambleType', values_from = 'meanAcc', names_prefix = 'acc_') %>%
  slice(rep(1:n(), each = 64))
memoryPredict <- memoryTestData %>%
  arrange(subject, pair) %>%
  cbind(predictMatch[, 3:5]) 

memoryPredictCount <- memoryPredict %>%
  filter(response != 'no response detected') %>% #filter out no response trials
  mutate(sdt = case_when(picType == 'foil' & acc == 1 ~ 'cr', picType == 'foil' & acc == 0 ~ 'fa', picType == 'old' & acc == 1 ~ 'hit', picType == 'old' & acc == 0 ~ 'miss') ) %>%
  group_by(subject,type, pair,sdt, predictType) %>%
  summarise(acc_firstSame = mean(acc_firstSame), acc_secondSame = mean(acc_secondSame), acc_swapping = mean(acc_swapping),n = n()) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n')
memoryPredictCount[,8:11][is.na(memoryPredictCount[,8:11])]<-0

## Hit_Rate : memory + predict
memoryPredictHit <- memoryPredictCount %>%
  mutate(hit_rate = n_hit/(n_hit + n_miss))
memoryPredictHitModel1 <- lmer(hit_rate ~ type * predictType * acc_firstSame * acc_secondSame * acc_swapping + (1|subject), data = memoryPredictHit)
anova(memoryPredictHitModel1)
summary(memoryPredictHitModel)

memoryPredictHit2 <- memoryPredictCount %>%
  mutate(hit_rate = n_hit/(n_hit + n_miss), meanPredictAcc = sum(acc_firstSame, acc_secondSame, acc_swapping)/3) 
memoryPredictHitModel2 <- lmer(hit_rate ~ type * predictType * meanPredictAcc + (1|subject), data = memoryPredictHit2)
anova(memoryPredictHitModel2)
summary(memoryPredictHitModel2)
ggscatter(memoryPredictHit2, x = 'meanPredictAcc', y = 'hit_rate', color = 'type', facet.by = 'predictType', add = 'reg.line', conf.int = T, cor.coef = T)


## Fa_Rate: memory + predict
memoryPredictFa <- memoryPredictCount %>%
  mutate(fa_rate = n_fa/(n_cr + n_fa), meanPredictAcc = sum(acc_firstSame, acc_secondSame, acc_swapping)/3)
memoryPredictFaModel1 <- lmer(fa_rate ~ type * predictType * acc_firstSame * acc_secondSame * acc_swapping + (1|subject), data = memoryPredictFa)
summary(memoryPredictFaModel1)

memoryPredictFaModel2 <- lmer(fa_rate ~ type * predictType * meanPredictAcc + (1|subject), data = memoryPredictFa)
summary(memoryPredictFaModel2)
anova(memoryPredictFaModel2)
ggscatter(memoryPredictFa, x = 'meanPredictAcc', y = 'fa_rate', color = 'predictType', facet.by = 'type', add = 'reg.line', conf.int = T, cor.coef = T)

## dprime: memory + predict
indicesPredictMemory <- psycho::dprime(memoryPredictCount$n_hit,memoryPredictCount$n_fa,memoryPredictCount$n_miss,memoryPredictCount$n_cr)
dprimeMemoryPredict <- cbind(memoryPredictCount, indicesPredictMemory)
dprimeMemoryPredict <- dprimeMemoryPredict %>%
  mutate(meanPredictAcc = sum(acc_firstSame, acc_secondSame, acc_swapping/3))
dprimeMemoryPredictModel1 <- lmer(dprime ~ type * predictType * acc_firstSame * acc_secondSame * acc_swapping + (1|subject), data = dprimeMemoryPredict)
summary(dprimeMemoryPredictModel1)

dprimeMemoryPredictModel2 <- lmer(dprime ~ type * predictType * meanPredictAcc + (1|subject), data = dprimeMemoryPredict)
anova(dprimeMemoryPredictModel2)
summary(dprimeMemoryPredictModel2)
ggscatter(dprimeMemoryPredict, x = 'meanPredictAcc', y = 'dprime', color = 'predictType', facet.by = 'type', add = 'reg.line', conf.int = T, cor.coef = T)

######## Control: filler memory + predict ########



