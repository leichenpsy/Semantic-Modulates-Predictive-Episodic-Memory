library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(psych)
library(ggsignif)
library(emmeans)
library(ggpubr)

setwd('/Users/leichen/Research project/Predictive memory/Phonologyv3 Data (2022.04.14 - 2022.04.21)')
n_subject <- length(list.files('/Users/leichen/Research project/Predictive memory/Phonologyv3 Data (2022.04.14 - 2022.04.21)'))
data_all <- list.files(path = '/Users/leichen/Research project/Predictive memory/Phonologyv3 Data (2022.04.14 - 2022.04.21)') %>%
  lapply(read.csv) %>%
  bind_rows()
subjectExcluded <- c(23)

## Define the variables
coln_trans <- c('correctResponse','currSession','currTask','currTrial','pairCaType', 'pairSiType','picType','predictPair','predictType','response','responseTime','round','sessionNum','sizeType','stimulus','type','scrambleType','sessionStartTime','sessionEndTime','subject','expStartTime','expEndTime')


########## Subset Encoding Session Data #############

encodeData <- data_all %>%
  filter(currTask == 'encodePictures')
encodeData <- encodeData[,coln_trans] %>%
  mutate(acc = ifelse(correctResponse == response,1,0)) %>%
  mutate(category = substr(stimulus,1,1)) %>%
  mutate(subCategory = substr(stimulus,1,2)) %>%
  group_by(subject) %>%
  mutate(repFiller = ifelse(subCategory == lag(subCategory),1,0)) %>%
  ungroup()
encodeData[,26][is.na(encodeData[,26])] <- 0

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

quartile <- encodeData[,c('subject','currTrial','stimulus','repFiller')] %>%
  group_by('subject') %>%
  arrange('currTrial') %>%
  cbind(q = rep(1:4, times = n_subject, each = 64), half = rep(1:2, times = n_subject, each = 128))
encodeData <- merge(encodeData, quartile[,c('subject','q','stimulus', 'half')],by = c('subject','stimulus'))
memoryData <- merge(memoryData,quartile[,c('subject','q','stimulus', 'half','repFiller')],by = c('subject','stimulus'),all = TRUE)

## mark memory response as hit, miss, fa, cr

memoryData <- memoryData %>%
  filter(response != 'no response detected') %>% #filter out no response trials
  mutate(sdt = case_when(picType == 'foil' & acc == 1 ~ 'cr', picType == 'foil' & acc == 0 ~ 'fa', picType == 'old' & acc == 1 ~ 'hit', picType == 'old' & acc == 0 ~ 'miss') )

############ Subset predict Data

predictData <- data_all[,coln_trans] %>%
  filter(currTask == 'predictTest' & correctResponse != 'no correct response' & response != 'no response detected') %>%
  mutate(acc = ifelse(response == correctResponse,1,0),
         pairCaType = ifelse(substr(predictPair,1,1) == substr(predictPair,3,3),'within','across'))



################# Cover Task Analysis ################


############# Acc Analysis #################

coverTaskAcc <- encodeData %>%
  group_by(subject) %>%
  filter(repFiller != 1) %>%
  summarize(acc = mean(acc))

describe(coverTaskAcc$acc) ###### basic acc info

#coverTaskAcc$acc > 0.91 + 3*0.07
#listSE <- coverTaskAcc$acc < 0.91 - 3*0.07

#subjectExcluded <- coverTaskAcc$subject[listSE] # No subject was excluded

coverTaskRepFiller <- encodeData %>%
  filter(!subject %in% subjectExcluded & repFiller == 1) %>%
  summarise(count = n()/((n_subject - length(subjectExcluded)) * 256))

coverTaskAcc <- encodeData %>%
  filter(!subject %in% subjectExcluded & repFiller != 1) %>%
  group_by(subject, type, predictType) %>%
  summarise(acc = mean(acc))

coverTaskAccModel <- lmer(acc ~ type * predictType + (1|subject), data = coverTaskAcc)
anova(coverTaskAccModel)

p <- ggplot(data = coverTaskAcc, aes(x = type, y = acc, color = subject)) + geom_point(alpha = 0.2, position =  position_dodge(width = 0.7)) +geom_line (aes(group = subject), alpha = 0.2)+ stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.5, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(aes(group = 1),fun = 'mean', geom = 'line')+ theme_classic()+ facet_wrap(~predictType)
p + scale_y_continuous(breaks = seq(0.6,1,by = 0.1))

emCoverTaskAcc <- emmeans(coverTaskAccModel, ~type * predictType)
contrast(emCoverTaskAcc, 'pairwise', by = 'predictType')
contrast(emCoverTaskAcc, 'pairwise', by = 'type')


coverTaskAccFlat <- coverTaskAcc %>%
  pivot_wider(names_from = c('type', 'predictType'), values_from = 'acc')
t.test(coverTaskAccFlat$filler_NA, coverTaskAccFlat$target_within, paired = TRUE)
t.test(coverTaskAccFlat$filler_NA, coverTaskAccFlat$target_cross, paired = TRUE)
t.test(coverTaskAccFlat$filler_NA, coverTaskAccFlat$cue_within, paired = TRUE)
t.test(coverTaskAccFlat$filler_NA, coverTaskAccFlat$cue_cross, paired = TRUE)

#### Compare two halves ####

coverTaskAccH <- encodeData %>%
  filter(!subject %in% subjectExcluded & repFiller != 1) %>%
  mutate(half = factor(half)) %>%
  group_by(subject, type, predictType, half) %>%
  summarise(acc = mean(acc))
  

coverTaskAccHModel <- lmer(acc ~ type * predictType * half + (1|subject), data = coverTaskAccH)
anova(coverTaskAccHModel)

half <- c(`1` = '1st half', `2` = '2nd half')
ggplot(data = coverTaskAccH, aes(x = type, y = acc, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(alpha = 0.2, aes(group = subject)) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.3, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(fun = 'mean', geom = 'line', aes(group = 1)) + facet_grid(half ~ predictType) + theme_bw() + theme(strip.background =element_rect(fill="white"), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

emcoverTaskAccH <- emmeans(coverTaskAccHModel, ~type * predictType * half)
contrast(emcoverTaskAccH, 'pairwise', by = 'half' )

coverTaskAccHFlat <- coverTaskAccH%>%
  pivot_wider(names_from = c('type', 'predictType','half'), values_from = 'acc')
t.test(coverTaskAccHFlat$filler_NA_1, coverTaskAccHFlat$target_within_1, paired = T)
t.test(coverTaskAccHFlat$filler_NA_1, coverTaskAccHFlat$target_cross_1, paired = T)
t.test(coverTaskAccHFlat$filler_NA_1, coverTaskAccHFlat$cue_within_1, paired = T)
t.test(coverTaskAccHFlat$filler_NA_1, coverTaskAccHFlat$cue_cross_1, paired = T)

t.test(coverTaskAccHFlat$filler_NA_2, coverTaskAccHFlat$target_within_2, paired = T)
t.test(coverTaskAccHFlat$filler_NA_2, coverTaskAccHFlat$target_cross_2, paired = T)
t.test(coverTaskAccHFlat$filler_NA_2, coverTaskAccHFlat$cue_within_2, paired = T)
t.test(coverTaskAccHFlat$filler_NA_2, coverTaskAccHFlat$cue_cross_2, paired = T)

#### Compare 4 quartiles ####
coverTaskAccQ <- encodeData %>%
  filter(!subject %in% subjectExcluded & repFiller != 1) %>%
  mutate(q = factor(q)) %>%
  group_by(subject, type, predictType, q) %>%
  summarise(acc = mean(acc)) 
 
  
coverTaskAccQModel <- lmer(acc ~ type * predictType * q + (1|subject), data = coverTaskAccQ)
anova(coverTaskAccQModel)

quartile <- c(`1` = '1st quartile', `2` = '2nd quartile', `3` = '3rd quartile', `4` = '4th quartile')
ggplot(data = coverTaskAccQ, aes(x = type, y = acc, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(aes(group = subject), alpha = 0.2) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.3, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(fun = 'mean', aes(group = 1), geom = 'line') + facet_grid(vars(q), vars(predictType))+ theme_bw() + theme(strip.background =element_rect(fill="white"), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

emcoverTaskAccQ <- emmeans(coverTaskAccQModel, ~ type * predictType * q)
pairs(emcoverTaskAccQ, by = 'q')

coverTaskAccQFlat <- coverTaskAccQ %>%
  pivot_wider(names_from = c('predictType', 'type', 'q'), values_from = 'acc')

t.test(coverTaskAccQFlat$NA_filler_1, coverTaskAccQFlat$cross_target_1, paired = T)
t.test(coverTaskAccQFlat$NA_filler_1, coverTaskAccQFlat$within_target_1, paired = T)
t.test(coverTaskAccQFlat$NA_filler_1, coverTaskAccQFlat$cross_cue_1, paired = T)
t.test(coverTaskAccQFlat$NA_filler_1, coverTaskAccQFlat$within_cue_1, paired = T)

t.test(coverTaskAccQFlat$NA_filler_2, coverTaskAccQFlat$cross_target_2, paired = T)
t.test(coverTaskAccQFlat$NA_filler_2, coverTaskAccQFlat$within_target_2, paired = T)
t.test(coverTaskAccQFlat$NA_filler_2, coverTaskAccQFlat$cross_cue_2, paired = T)
t.test(coverTaskAccQFlat$NA_filler_2, coverTaskAccQFlat$within_cue_2, paired = T)

t.test(coverTaskAccQFlat$NA_filler_3, coverTaskAccQFlat$cross_target_3, paired = T)
t.test(coverTaskAccQFlat$NA_filler_3, coverTaskAccQFlat$within_target_3, paired = T)
t.test(coverTaskAccQFlat$NA_filler_3, coverTaskAccQFlat$cross_cue_3, paired = T)
t.test(coverTaskAccQFlat$NA_filler_3, coverTaskAccQFlat$within_cue_3, paired = T)

t.test(coverTaskAccQFlat$NA_filler_4, coverTaskAccQFlat$cross_target_4, paired = T)
t.test(coverTaskAccQFlat$NA_filler_4, coverTaskAccQFlat$within_target_4, paired = T)
t.test(coverTaskAccQFlat$NA_filler_4, coverTaskAccQFlat$cross_cue_4, paired = T)
t.test(coverTaskAccQFlat$NA_filler_4, coverTaskAccQFlat$within_cue_4, paired = T)

################ Response time analysis ##################
coverTaskRT <- encodeData %>%
  filter(!subject %in% subjectExcluded & repFiller != 1 & acc == 1) %>%
  mutate(rt = as.numeric(responseTime)) 
describe(coverTaskRT$rt)

coverTaskRT <- encodeData %>%
  filter(!subject %in% subjectExcluded & repFiller != 1 & acc == 1) %>%
  mutate(rt = as.numeric(responseTime)) %>%
  group_by(subject, type, predictType) %>%
  summarise(rt = median(rt))

coverTaskRTModel <- lmer(rt ~ type * predictType + (1|subject), data = coverTaskRT)
anova(coverTaskRTModel)

ggplot(data = coverTaskRT, aes(x = type, y = rt, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(aes(group = subject), alpha = 0.2) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.4, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(aes(group = 1), fun = 'mean', geom = 'line')+ theme_classic() + facet_wrap(~predictType)

emcoverTaskRT <- emmeans(coverTaskRTModel, ~ predictType * type)
contrast(emcoverTaskRT, by = 'type', 'pairwise')
contrast(emcoverTaskRT, by = 'predictType', "pairwise")

coverTaskRTFlat <- coverTaskRT %>%
  pivot_wider(names_from = c('type', 'predictType'), values_from = 'rt')
t.test(coverTaskRTFlat$filler_NA, coverTaskRTFlat$target_within, paired = T) 
t.test(coverTaskRTFlat$filler_NA, coverTaskRTFlat$target_cross, paired = T) 
t.test(coverTaskRTFlat$filler_NA, coverTaskRTFlat$cue_within, paired = T) 
t.test(coverTaskRTFlat$filler_NA, coverTaskRTFlat$cue_cross, paired = T) 

##### Compare two halves #####
coverTaskRTH <- encodeData %>%
  filter(!subject %in% subjectExcluded & repFiller != 1 & acc == 1) %>%
  mutate(rt = as.numeric(responseTime)) %>%
  mutate(half = factor(half)) %>%
  group_by(subject, type, predictType, half) %>%
  summarise(rt = median(rt)) 

coverTaskRTHModel <- lmer(rt ~ type * predictType * half + (1|subject), data = coverTaskRTH)
anova(coverTaskRTHModel)

ggplot(data = coverTaskRTH, aes(x = type, y = rt, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(aes(group = subject), alpha = 0.2) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.3, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(aes(group = 1), fun = 'mean',geom = 'line') + facet_grid(vars(half), vars(predictType)) + theme_bw() + theme(strip.background =element_rect(fill="white", color = 'black', size = 1), panel.border = element_rect(color = 'black', size = 1), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

emcoverTaskRTH <- emmeans(coverTaskRTHModel, ~type * predictType * half)
contrast(emcoverTaskRTH, 'pairwise', by = 'half' )

coverTaskRTHFlat <- coverTaskRTH %>%
  pivot_wider(names_from = c('type', 'predictType', 'half'), values_from = 'rt')
t.test(coverTaskRTHFlat$filler_NA_1, coverTaskRTHFlat$target_within_1, paired = T)
t.test(coverTaskRTHFlat$filler_NA_1, coverTaskRTHFlat$target_cross_1, paired = T)
t.test(coverTaskRTHFlat$filler_NA_1, coverTaskRTHFlat$cue_cross_1, paired = T)
t.test(coverTaskRTHFlat$filler_NA_1, coverTaskRTHFlat$cue_within_1, paired = T)

t.test(coverTaskRTHFlat$filler_NA_2, coverTaskRTHFlat$target_cross_2, paired = T)
t.test(coverTaskRTHFlat$filler_NA_2, coverTaskRTHFlat$cue_cross_2, paired = T)
t.test(coverTaskRTHFlat$filler_NA_2, coverTaskRTHFlat$target_within_2, paired = T)
t.test(coverTaskRTHFlat$filler_NA_2, coverTaskRTHFlat$cue_within_2, paired = T)

#### Compare 4 quartiles ####
coverTaskRTQ <- encodeData %>%
  filter(!subject %in% subjectExcluded & repFiller != 1 & acc == 1) %>%
  mutate(rt = as.numeric(responseTime)) %>%
  mutate(q = factor(q)) %>%
  group_by(subject, type, predictType, q) %>%
  summarise(rt = median(rt)) 



coverTaskRTQModel <- lmer(rt~type * predictType * q + (1|subject), data = coverTaskRTQ)
anova(coverTaskRTQModel)

ggplot(data = coverTaskRTQ, aes(x = type, y = rt, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(aes(group = subject), alpha = 0.2) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.3, alpha = 1, position = position_dodge(width = 0.7))+ stat_summary(fun = 'mean', geom = 'line', aes(group = 1))+ facet_grid(vars(q), vars(predictType)) + theme_classic()

emcoverTaskRTQ <- emmeans(coverTaskRTQModel, ~ type * predictType * q)
pairs(emcoverTaskAccQ, by = 'q')

coverTaskRTQFlat <- coverTaskRTQ %>%
  pivot_wider(names_from = c('type','predictType','q'), values_from = 'rt')
t.test(coverTaskRTQFlat$filler_NA_1, coverTaskRTQFlat$target_within_1, paired = T)
t.test(coverTaskRTQFlat$filler_NA_1, coverTaskRTQFlat$cue_within_1, paired = T)
t.test(coverTaskRTQFlat$filler_NA_1, coverTaskRTQFlat$target_cross_1, paired = T)
t.test(coverTaskRTQFlat$filler_NA_1, coverTaskRTQFlat$cue_cross_1, paired = T)

t.test(coverTaskRTQFlat$filler_NA_2, coverTaskRTQFlat$target_within_2, paired = T)
t.test(coverTaskRTQFlat$filler_NA_2, coverTaskRTQFlat$cue_within_2, paired = T)
t.test(coverTaskRTQFlat$filler_NA_2, coverTaskRTQFlat$target_cross_2, paired = T)
t.test(coverTaskRTQFlat$filler_NA_2, coverTaskRTQFlat$cue_cross_2, paired = T)

t.test(coverTaskRTQFlat$filler_NA_3, coverTaskRTQFlat$target_within_3, paired = T)
t.test(coverTaskRTQFlat$filler_NA_3, coverTaskRTQFlat$cue_within_3, paired = T)
t.test(coverTaskRTQFlat$filler_NA_3, coverTaskRTQFlat$target_cross_3, paired = T)
t.test(coverTaskRTQFlat$filler_NA_3, coverTaskRTQFlat$cue_cross_3, paired = T)

t.test(coverTaskRTQFlat$filler_NA_4, coverTaskRTQFlat$target_within_4, paired = T)
t.test(coverTaskRTQFlat$filler_NA_4, coverTaskRTQFlat$cue_within_4, paired = T)
t.test(coverTaskRTQFlat$filler_NA_4, coverTaskRTQFlat$target_cross_4, paired = T)
t.test(coverTaskRTQFlat$filler_NA_4, coverTaskRTQFlat$cue_cross_4, paired = T)




################## SLP Index ##################
logTrans <- encodeData %>%
  mutate(rt = case_when(acc == 1 ~ as.numeric(responseTime),acc == 0 ~ 0),trialNo = as.numeric(currTrial), logTime = log10(rt))
diff <- logTrans %>%
  group_by(subject) %>%
  arrange(trialNo, .by_group = TRUE) %>%
  mutate(diff = logTime - lead(logTime, default = NA))

SLP <- diff %>%
  mutate(diffType = case_when(type == 'cue' ~ 'cue-target', type == 'target' & lead(type) == 'filler' ~ 'target-filler', type == 'target' & lead(type) == 'cue' ~ 'target-cue', type == 'filler' & lead(type) == 'filler' ~ 'filler-filler', type == 'filler' & lead(type) == 'cue' ~ 'filler-cue')) %>%
  mutate(diffCa = case_when(category == lead(category) ~ 'withinCa', category != lead(category) ~ 'acrossCa'))%>%
  mutate(fillerCa = case_when(subCategory == lead(subCategory) ~ 'same', subCategory != lead(subCategory) ~ 'diff')) %>%
  mutate(diffType = ifelse(diffType == 'filler-filler' & fillerCa == 'same', 'filler-filler(same)', diffType))%>%
  filter(!is.na(diff) & diff != -Inf & diff != Inf)

meanSLP <- SLP %>%
  filter(!subject %in% subjectExcluded & fillerCa != 'same')%>%
  filter(diffType == 'filler-filler' | diffType == 'cue-target') %>%
  group_by(subject, diffType, diffCa) %>%
  summarise(meanSLP = mean(diff))

describe(meanSLP$meanSLP)

meanSLPModel <- lmer(meanSLP ~ diffCa * diffType + (1|subject), data = meanSLP)
anova(meanSLPModel)

diffCa <- c(`acrossCa` = 'across category', `withinCa` = 'within category')
ggplot(data = meanSLP, aes(x = diffType, y = meanSLP, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(aes(group = subject), alpha = 0.2) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.3, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(aes(group = 1), fun = "mean", geom = "line", size = 0.4, position = position_dodge(width = 0.7)) + theme_classic() + facet_wrap(~diffCa, labeller = as_labeller(diffCa)) + theme(strip.background = element_rect(color = 'white'),legend.position = "none") + geom_hline(yintercept = 0, linetype="dashed", color = "black", alpha = 0.8, size = 0.2)


emmeanSLP <- emmeans(meanSLPModel, ~ diffCa * diffType)
contrast(emmeanSLP, by = 'diffCa', 'pairwise')
contrast(emmeanSLP, by = 'diffType', 'pairwise')

meanSLPFlat <- meanSLP %>%
  pivot_wider(names_from = c('diffCa', 'diffType'), values_from = 'meanSLP')
t.test(meanSLPFlat$`acrossCa_filler-filler`, mu = 0)
t.test(meanSLPFlat$`withinCa_filler-filler`, mu = 0)
t.test(meanSLPFlat$`acrossCa_cue-target`, mu = 0)
t.test(meanSLPFlat$`withinCa_cue-target`, mu = 0)

#### Compare two halves ####

meanSLPH <- SLP %>%
  filter(!subject %in% subjectExcluded & fillerCa != 'same')%>%
  filter(diffType == 'filler-filler' | diffType == 'cue-target') %>%
  mutate(half = factor(half)) %>%
  group_by(subject, diffType, diffCa, half) %>%
  summarise(meanSLP = mean(diff)) 
 


meanSLPHModel <- lmer(meanSLP ~ diffType * diffCa * half + (1|subject), data = meanSLPH)
anova(meanSLPHModel)

ggplot(data = meanSLPH, aes(x = diffType, y = meanSLP, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(aes(group = subject), alpha = 0.2) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.3, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(aes(group = diffCa), fun = "mean", geom = "line", size = 0.5) + facet_grid(half~diffCa) + theme_classic() + theme(strip.background = element_rect(color = 'white'),legend.position = "none") + geom_hline(yintercept = 0, linetype="dashed", color = "black", alpha = 0.8, size = 0.2)

emSLPH <-emmeans(meanSLPHModel, ~~ diffType * diffCa * half)
contrast(emSLPH, by = 'half', 'pairwise')

meanSLPHFlat <- meanSLPH %>%
  pivot_wider(names_from = c('diffType','diffCa','half'), values_from = 'meanSLP')
t.test(meanSLPHFlat$`cue-target_withinCa_1`, mu = 0)
t.test(meanSLPHFlat$`cue-target_withinCa_2`, mu = 0)
t.test(meanSLPHFlat$`filler-filler_withinCa_1`, mu = 0)
t.test(meanSLPHFlat$`filler-filler_withinCa_2`, mu = 0)

t.test(meanSLPHFlat$`cue-target_acrossCa_1`, mu = 0)
t.test(meanSLPHFlat$`cue-target_acrossCa_2`, mu = 0)
t.test(meanSLPHFlat$`filler-filler_acrossCa_1`, mu = 0)
t.test(meanSLPHFlat$`filler-filler_acrossCa_2`, mu = 0)

#### Compare 4 quartiles ####

meanSLPQ <- SLP %>%
  filter(!subject %in% subjectExcluded & fillerCa != 'same')%>%
  filter(diffType == 'filler-filler' | diffType == 'cue-target') %>%
  mutate(q = factor(q)) %>%
  group_by(subject, diffType, diffCa, q) %>%
  summarise(meanSLP = mean(diff))


meanSLPQModel <- lmer(meanSLP ~ diffType * diffCa * q + (1|subject), data = meanSLPQ)
anova(meanSLPQModel)

ggplot(data = meanSLPQ, aes(x = diffType, y = meanSLP, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(aes(group = subject), alpha = 0.2) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.3, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(aes(group = diffCa), fun = "mean", geom = "line", size = 0.5) + facet_grid(q~diffCa) + theme_classic() + theme(strip.background = element_rect(color = 'white'),legend.position = "none") + geom_hline(yintercept = 0, linetype="dashed", color = "black", alpha = 0.8, size = 0.2)




emSLPQ <- emmeans(meanSLPQModel, ~diffType * diffCa*q)
contrast(emSLPQ, by = 'q', 'pairwise')

SLPQFlat <- meanSLPQ %>%
  pivot_wider(names_from = c('diffCa','diffType', 'q'), values_from = 'meanSLP')
t.test(SLPQFlat$`acrossCa_cue-target_1`, mu = 0)
t.test(SLPQFlat$`withinCa_cue-target_1`, mu = 0)
t.test(SLPQFlat$`acrossCa_filler-filler_1`, mu = 0)
t.test(SLPQFlat$`withinCa_filler-filler_1`, mu = 0)

t.test(SLPQFlat$`acrossCa_cue-target_2`, mu = 0)
t.test(SLPQFlat$`withinCa_cue-target_2`, mu = 0)
t.test(SLPQFlat$`acrossCa_filler-filler_2`, mu = 0)
t.test(SLPQFlat$`withinCa_filler-filler_2`, mu = 0)

t.test(SLPQFlat$`acrossCa_cue-target_3`, mu = 0)
t.test(SLPQFlat$`withinCa_cue-target_3`, mu = 0)
t.test(SLPQFlat$`acrossCa_filler-filler_3`, mu = 0)
t.test(SLPQFlat$`withinCa_filler-filler_3`, mu = 0)

t.test(SLPQFlat$`acrossCa_cue-target_4`, mu = 0)
t.test(SLPQFlat$`withinCa_cue-target_4`, mu = 0)
t.test(SLPQFlat$`acrossCa_filler-filler_4`, mu = 0)
t.test(SLPQFlat$`withinCa_filler-filler_4`, mu = 0)

################# Memory Test Analysis ################

memoryTest <- memoryData %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(repFiller == 0 | is.na(repFiller)) %>%
  group_by(subject) %>%
  summarise(acc = mean(acc))
describe(memoryTest$acc)


memoryOld <- memoryData %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(repFiller == 0 | is.na(repFiller)) %>%
  filter(picType == 'old') %>%
  group_by(subject, predictType, type) %>%
  summarise(hit_rate = mean(acc)) %>%
  mutate(z_hit = qnorm(hit_rate))

memoryFoil <- memoryData %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(repFiller == 0 | is.na(repFiller)) %>%
  filter(picType == 'foil') %>%
  group_by(subject, predictType, type) %>%
  summarise(fa_rate = 1- mean(acc)) %>%
  mutate(z_fa = qnorm(fa_rate))

memoryDprime <- merge(memoryOld, memoryFoil, by = c('subject', 'predictType','type'))
memoryDprime <- memoryDprime %>%
  mutate(dprime = z_hit - z_fa)

####### Check Inf Results ######
fa_0_subject <- c(26, 37)

memoryDprime <- memoryDprime %>%
  filter(!subject %in% fa_0_subject)
describe(memoryDprime$dprime)

ggplot(data = memoryDprime, aes(x = type, y = dprime, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(aes(group = subject), alpha = 0.2)+ stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.5, alpha = 1, position = position_dodge(width = 0.7))+ stat_summary(fun = 'mean', aes(group = 1), geom = 'line') + theme_classic() + facet_wrap(~predictType)

memoryDprimeModel <- lmer(dprime ~ predictType * type + (1|subject), data = memoryDprime)
anova(memoryDprimeModel)

emDprime <- emmeans(memoryDprimeModel, ~predictType * type)
contrast(emDprime, by = 'predictType', 'pairwise')
contrast(emDprime, by = 'type', 'pairwise')

dprimeFlat <- memoryDprime %>%
  pivot_wider(id_cols = 'subject', names_from = c('predictType', 'type'), values_from = 'dprime')

t.test(dprimeFlat$cross_cue, dprimeFlat$NA_filler, paired = T)
t.test(dprimeFlat$cross_target, dprimeFlat$NA_filler, paired = T)
t.test(dprimeFlat$within_cue, dprimeFlat$NA_filler, paired = T)
t.test(dprimeFlat$within_target, dprimeFlat$NA_filler, paired = T)

diffDprime <- dprimeFlat %>%
  mutate(cue_cross = cross_cue- NA_filler, cue_within = within_cue - NA_filler, target_cross = cross_target - NA_filler, target_within = within_target - NA_filler) %>%
  pivot_longer(cue_cross:target_within, names_to = c('type','predictType'), names_sep = '_', values_to = 'diff_dprime')

diffDprimeModel <- lmer(diff_dprime ~ type * predictType + (1|subject), data = diffDprime)
anova(diffDprimeModel)

ggplot(data = diffDprime, aes(x = type, y = diff_dprime, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(aes(group = subject), alpha = 0.2)+ stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.5, alpha = 1, position = position_dodge(width = 0.7))+ stat_summary(fun = 'mean', aes(group = 1), geom = 'line') + theme_classic() + facet_wrap(~predictType)

##### Pairing SLP index and the dprime results #####
pairSLP <- meanSLP %>%
  filter(!subject %in% fa_0_subject) %>%
  filter(diffType == 'cue-target') %>%
  mutate(predictType = ifelse(diffCa == 'acrossCa', 'cross','within'))

pairDprime <- memoryDprime %>%
  filter(type != 'filler')

matchDprime <- merge(pairDprime, pairSLP, by = c('subject', 'predictType'))

matchDprimeModel<- lmer(dprime ~ type * predictType * meanSLP + (1|subject), data = matchDprime)
anova(matchDprimeModel)
summary(matchDprimeModel)

ggscatter(matchDprime, x = 'meanSLP', y = 'dprime', color = 'predictType', add = 'reg.line', conf.int = T, cor.coef = T) + facet_grid(predictType ~ type)

matchDprimeDiff <- merge(diffDprime, pairSLP, by = c('subject', 'predictType'))
matchDprimeDiffModel <- lmer(diff_dprime ~ type * predictType * meanSLP + (1|subject), data = matchDprimeDiff)
anova(matchDprimeDiffModel)
ggscatter(matchDprimeDiff, x = 'meanSLP', y = 'diff_dprime', color = 'predictType', add = 'reg.line', conf.int = T, cor.coef = T) + facet_grid(predictType ~ type)

##### divide dprime results based on meanSLP score ######

rateSLP <- SLP %>%
  filter(!subject %in% subjectExcluded & fillerCa != 'same')%>%
  filter(diffType == 'filler-filler' | diffType == 'cue-target') %>%
  filter(!subject %in% fa_0_subject) %>%
  filter(diffType == 'cue-target') %>%
  group_by(subject) %>%
  summarise(meanSLP = mean(diff)) %>%
  mutate(group = ifelse(meanSLP < median(meanSLP), 'Low SLP','High SLP'))

rateDprime <- merge(memoryDprime, rateSLP, by = c('subject'))

rateDprimeModel <- lmer(dprime ~ type * predictType * group + (1|subject), data = rateDprime)
anova(rateDprimeModel)

ggplot(data = rateDprime, aes(x = type, y = dprime, color = predictType)) + geom_point(alpha = 0.2, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.5, alpha = 1, position = position_dodge(width = 0.7)) + theme_classic() + facet_wrap(~group)

######### Memory Hit Analysis ########

memoryHit <- memoryData %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(repFiller == 0 | is.na(repFiller)) %>%
  filter(picType == 'old') %>%
  group_by(subject, predictType, type) %>%
  summarise(hit_rate = mean(acc)) 

memoryHitModel <- lmer(hit_rate ~ type * predictType + (1|subject), data = memoryHit)
anova(memoryHitModel)

ggplot(data = memoryHit, aes(x = type, y = hit_rate, color = predictType)) + geom_point(alpha = 0.2, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.5, alpha = 1, position = position_dodge(width = 0.7)) + theme_classic() 

emHit <- emmeans(memoryHitModel, ~type * predictType)
contrast(emHit, by = 'type', 'pairwise')
contrast(emHit, by = 'predictType', 'pairwise')

memoryHitFlat <- memoryHit %>%
  pivot_wider(names_from = c('type','predictType'), values_from = 'hit_rate')
t.test(memoryHitFlat$cue_cross, memoryHitFlat$filler_NA, paired = T)
t.test(memoryHitFlat$cue_within, memoryHitFlat$filler_NA, paired = T)
t.test(memoryHitFlat$target_cross, memoryHitFlat$filler_NA, paired = T)
t.test(memoryHitFlat$target_within, memoryHitFlat$filler_NA, paired = T)

#### Compare two halves #####

memoryHitH <- memoryData %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(repFiller == 0 |is.na(repFiller)) %>%
  filter(picType == 'old') %>%
  group_by(subject, predictType, type, half) %>%
  summarise(hit_rate = mean(acc)) 

memoryHitHModel <- lmer(hit_rate ~ type * predictType * half + (1|subject), data = memoryHitH)
anova(memoryHitHModel)

ggplot(data = memoryHitH, aes(x = type, y = hit_rate, color = predictType)) + geom_point(alpha = 0.2, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.5, alpha = 1, position = position_dodge(width = 0.7)) + theme_classic() + facet_wrap(~half, labeller = as_labeller(half)) + theme_bw() + theme(strip.background =element_rect(fill="white", color = 'black', size = 1), panel.border = element_rect(color = 'black', size = 1), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

emHitH <- emmeans(memoryHitHModel, ~type * predictType * half)
contrast(emHitH, by = 'half', 'pairwise')

##### Compare 4 quartiles ######

memoryHitQ <- memoryData %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(repFiller == 0 | is.na(repFiller)) %>%
  filter(picType == 'old') %>%
  group_by(subject, predictType, type, q) %>%
  summarise(hit_rate = mean(acc)) 

memoryHitQModel <- lmer(hit_rate ~ type * predictType * q + (1|subject), data = memoryHitQ)
anova(memoryHitQModel)

ggplot(data = memoryHitQ, aes(x = type, y = hit_rate, color = predictType)) + geom_point(alpha = 0.2, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.5, alpha = 1, position = position_dodge(width = 0.7)) + theme_classic() + facet_wrap(~q, ncol = 4, labeller = as_labeller(quartile)) + theme_bw() + theme(strip.background =element_rect(fill="white", color = 'black', size = 1), panel.border = element_rect(color = 'black', size = 1), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

########### False Alarm Analysis ########### 

memoryFa <- memoryData %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(repFiller == 0 | is.na(repFiller)) %>%
  filter(picType == 'foil') %>%
  group_by(subject, predictType, type) %>%
  summarise(fa_rate = 1-mean(acc)) 

memoryFaModel <- lmer(fa_rate ~ type * predictType + (1|subject), data = memoryFa)
anova(memoryFaModel)

ggplot(data = memoryFa, aes(x = type, y = fa_rate, color = predictType)) + geom_point(alpha = 0.2, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.5, alpha = 1, position = position_dodge(width = 0.7)) + theme_classic() 

emFa <- emmeans(memoryFaModel, ~ type * predictType)
contrast(emFa, by = 'type', 'pairwise')
contrast(emFa, by = 'predictType','pairwise')

memoryFaFlat <- memoryFa %>%
  pivot_wider(names_from = c('type', 'predictType'), values_from = fa_rate)
t.test(memoryFaFlat$filler_NA, memoryFaFlat$target_cross, paired = T)
t.test(memoryFaFlat$filler_NA, memoryFaFlat$target_within, paired = T)
t.test(memoryFaFlat$filler_NA, memoryFaFlat$cue_within, paired = T)


###### Compare Dprime two halves ######

memoryOldH <- memoryData %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(repFiller == 0 | is.na(repFiller)) %>%
  filter(picType == 'old') %>%
  mutate(half = factor(half)) %>%
  group_by(subject, predictType, type, half) %>%
  summarise(hit_rate = mean(acc)) %>%
  mutate(z_hit = qnorm(hit_rate))
pairDprimeH <- merge(memoryOldH, memoryFoil, by = c('subject','type','predictType'))

### Check Inf value ###
hit_0_subject <- c(14)

memoryDprimeH <- pairDprimeH %>%
  mutate(dprime = z_hit - z_fa) %>%
  filter(!subject %in% fa_0_subject) %>%
  filter(!subject %in% hit_0_subject)
  

memoryDprimeHModel <- lmer(dprime ~ predictType * type * half  + (1|subject), data = memoryDprimeH)
anova(memoryDprimeHModel)

ggplot(data = memoryDprimeH, aes(x = type, y = dprime, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7))+ geom_line(aes(group = subject), alpha = 0.2) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.5, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(fun = 'mean', aes(group = 1), geom = 'line')+ theme_classic() + facet_grid(half~predictType) + theme_bw() + theme(strip.background =element_rect(fill="white", color = 'black', size = 1), panel.border = element_rect(color = 'black', size = 1), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

emDprimeH <- emmeans(memoryDprimeHModel, ~half*predictType*type)
contrast(emDprimeH, by = 'half', 'pairwise')

dprimeHFlat <- memoryDprimeH %>%
  pivot_wider(id_cols = 'subject',names_from = c('type', 'predictType','half'), values_from = 'dprime')
t.test(dprimeHFlat$filler_NA_1,dprimeHFlat$cue_cross_1, paired = T)
t.test(dprimeHFlat$filler_NA_1, dprimeHFlat$cue_within_1, paired = T)
t.test(dprimeHFlat$filler_NA_1, dprimeHFlat$target_within_1, paired = T)
t.test(dprimeHFlat$filler_NA_1, dprimeHFlat$target_cross_1, paired = T)

t.test(dprimeHFlat$filler_NA_2, dprimeHFlat$cue_cross_2, paired = T)
t.test(dprimeHFlat$filler_NA_2, dprimeHFlat$cue_within_2, paired = T)
t.test(dprimeHFlat$filler_NA_2, dprimeHFlat$target_within_2, paired = T)
t.test(dprimeHFlat$filler_NA_2, dprimeHFlat$target_cross_2, paired = T)

##### Compare 4 quartiles #####

memoryOldQ <- memoryData %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(repFiller == 0 | is.na(repFiller)) %>%
  mutate(q = factor(q)) %>%
  filter(picType == 'old') %>%
  group_by(subject, predictType, type, q) %>%
  summarise(hit_rate = mean(acc)) %>%
  mutate(z_hit = qnorm(hit_rate))

hit_0_subject <- c(3, 13, 14, 15, 16, 18, 19, 22, 26, 27, 30, 31, 36, 38, 39, 40, 41)

pairDprimeQ <- merge(memoryOldQ, memoryFoil, by = c('subject','type','predictType'))
memoryDprimeQ <- pairDprimeQ %>%
  mutate(dprime = z_hit - z_fa) %>%
  filter(!subject %in% fa_0_subject) %>%
  filter(!subject %in% hit_0_subject) 
  

memoryDprimeQModel <- lmer(dprime ~ predictType * type * q + (1|subject), data = memoryDprimeQ)
anova(memoryDprimeQModel)

ggplot(data = memoryDprimeQ, aes(x = type, y = dprime, color = subject)) + geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) + geom_line(aes(group = subject), alpha = 0.2)+ stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.3, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(aes(group = 1), fun = 'mean', geom = 'line') + theme_classic() + facet_grid(q~predictType) + theme_bw() + theme(strip.background =element_rect(fill="white", color = 'black', size = 1), panel.border = element_rect(color = 'black', size = 1), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

emDprimeQ <- emmeans(memoryDprimeQModel, ~predictType * type * q)
contrast(emDprimeQ, by = 'q', 'pairwise')

dprimeQFlat <- memoryDprimeQ %>%
  pivot_wider(id_cols = 'subject', names_from = c('type','predictType','q'), values_from = 'dprime')

t.test(dprimeQFlat$filler_NA_1, dprimeQFlat$cue_cross_1, paired = T)
t.test(dprimeQFlat$filler_NA_1, dprimeQFlat$cue_within_1, paired = T)
t.test(dprimeQFlat$filler_NA_1, dprimeQFlat$target_cross_1, paired = T)
t.test(dprimeQFlat$filler_NA_1, dprimeQFlat$target_within_1, paired = T)

t.test(dprimeQFlat$filler_NA_2, dprimeQFlat$cue_cross_2, paired = T)
t.test(dprimeQFlat$filler_NA_2, dprimeQFlat$cue_within_2, paired = T)
t.test(dprimeQFlat$filler_NA_2, dprimeQFlat$target_cross_2, paired = T)
t.test(dprimeQFlat$filler_NA_2, dprimeQFlat$target_within_2, paired = T)

t.test(dprimeQFlat$filler_NA_3, dprimeQFlat$cue_cross_3, paired = T)
t.test(dprimeQFlat$filler_NA_3, dprimeQFlat$cue_within_3, paired = T)
t.test(dprimeQFlat$filler_NA_3, dprimeQFlat$target_cross_3, paired = T)
t.test(dprimeQFlat$filler_NA_3, dprimeQFlat$target_within_3, paired = T)

t.test(dprimeQFlat$filler_NA_4, dprimeQFlat$cue_cross_4, paired = T)
t.test(dprimeQFlat$filler_NA_4, dprimeQFlat$cue_within_4, paired = T)
t.test(dprimeQFlat$filler_NA_4, dprimeQFlat$target_cross_4, paired = T)
t.test(dprimeQFlat$filler_NA_4, dprimeQFlat$target_within_4, paired = T)




##### Compare the last 3 quartiles #####

memoryOld3Q <- memoryData %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(repFiller == 0 | is.na(repFiller)) %>%
  filter(picType == 'old' & q !=1) %>%
  group_by(subject, predictType, type) %>%
  summarise(hit_rate = mean(acc)) %>%
  mutate(z_hit = qnorm(hit_rate))

#### Check inf value
hit_0_subject = c(14)

pairDprime3Q <- merge(memoryOld3Q, memoryFoil, by = c('subject','predictType','type'))
memoryDprime3Q <- pairDprime3Q %>%
  mutate(dprime = z_hit - z_fa) %>%
  filter(!subject %in% fa_0_subject) %>%
  filter(subject != 14)

memoryDprime3QModel <- lmer(dprime ~ type * predictType + (1|subject), data = memoryDprime3Q)
anova(memoryDprime3QModel)

ggplot(data = memoryDprime3Q, aes(x = type, y = dprime, color = predictType)) + geom_point(alpha = 0.2, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.5, alpha = 1, position = position_dodge(width = 0.7)) + theme_classic() 

############# SLP + Dprime + Time ##############


#### Compare two halves ####
pairSLPH <- meanSLPH %>%
  filter(!subject %in% fa_0_subject) %>%
  filter(diffType == 'cue-target') %>%
  mutate(predictType = ifelse(diffCa == 'acrossCa', 'cross','within')) %>%
  filter(!subject %in% fa_0_subject) %>%
  filter(!subject %in% hit_0_subject) 

pairDprimeH <- memoryDprimeH %>%
  filter(type != 'filler')
matchDprimeH <- merge(pairSLPH, pairDprimeH, by = c('subject','predictType','half'))


matchDprimeHModel <- lmer(dprime ~ type * predictType * half *meanSLP + (1|subject), data = matchDprimeH)
anova(matchDprimeHModel)

ggscatter(matchDprimeH, x = 'meanSLP', y = 'dprime',  facet.by = 'type', add = 'reg.line', conf.int = T, cor.coef = T, color = 'orange') + facet_grid(half~type * predictType) + theme_bw() + theme(strip.background =element_rect(fill="white", color = 'black', size = 1), panel.border = element_rect(color = 'black', size = 1), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


###### Compare four quartiles #######
pairSLPQ <- meanSLPQ %>%
  filter(!subject %in% fa_0_subject) %>%
  filter(diffType == 'cue-target') %>%
  mutate(predictType = ifelse(diffCa == 'acrossCa', 'cross','within')) %>%
  filter(!subject %in% fa_0_subject) %>%
  filter(!subject %in% hit_0_subject) 

pairDprimeQ <- memoryDprimeQ %>%
  filter(type != 'filler')
matchDprimeQ <- merge(pairSLPQ, pairDprimeQ, by = c('subject','predictType','q'))



matchDprimeQModel <- lmer(dprime ~ type * predictType * q* meanSLP + (1|subject), data = matchDprimeQ)
anova(matchDprimeQModel)

ggscatter(matchDprimeQ, x = 'meanSLP', y = 'dprime', color = 'orange',  add = 'reg.line', conf.int = T, cor.coef = T) + facet_grid(q~type * predictType) + theme_bw() + theme(strip.background =element_rect(fill="white", color = 'black', size = 1), panel.border = element_rect(color = 'black', size = 1), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

##### Combine last 3 quartiles ######

pairSLP3Q <- SLP %>%
  filter(!subject %in% subjectExcluded) %>%
  filter(diffType == 'cue-target' & q != 1) %>%
  mutate(predictType = ifelse(diffCa == 'acrossCa', 'cross','within')) %>%
  filter(!subject %in% fa_0_subject) %>%
  filter(subject != 14) %>%
  group_by(subject, diffType, predictType) %>%
  summarise(meanSLP = mean(diff)) 

pairDprime3Q <- memoryDprime3Q %>%
  filter(type != 'filler') %>%
  filter(subject != 14)

matchDprime3Q <- merge(pairSLP3Q, pairDprime3Q, by = c('subject', 'predictType'))

matchDprime3QModel <- lmer(dprime ~ type * predictType * meanSLP + (1|subject), data = matchDprime3Q)
anova(matchDprime3QModel)

ggscatter(matchDprime3Q, x = 'meanSLP', y = 'dprime', color = 'predictType', add = 'reg.line', conf.int = T, cor.coef = T) + facet_grid(vars(predictType),vars(type)) + theme_bw() + theme(strip.background =element_rect(fill="white", color = 'black', size = 1), panel.border = element_rect(color = 'black', size = 1), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

################ Order Knowledge Test ##################
predictTest <- predictData %>%
  filter(!subject %in% subjectExcluded) %>%
  group_by(subject) %>%
  summarise(acc = mean(acc))
describe(predictTest$acc)

predictTest <- predictData %>%
  filter(!subject %in% subjectExcluded) %>%
  group_by(subject, pairCaType, scrambleType) %>%
  summarise(acc = mean(acc))

predictTestModel <- lmer(acc ~ pairCaType * scrambleType + (1|subject), data = predictTest)
anova(predictTestModel)

ggplot(data = predictTest, aes(x = scrambleType, y = acc, color = subject)) + geom_point(alpha = 0.2, position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.2)) +  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", size = 0.3, alpha = 1, position = position_dodge(width = 0.7)) + stat_summary(aes(group = 1), geom = 'line', fun = 'mean')+ theme_classic() + geom_hline(yintercept = 0.5, linetype="dashed", color = "black", alpha = 0.3) + facet_wrap(~pairCaType)

emPredict <- emmeans(predictTestModel, ~ pairCaType * scrambleType)
contrast(emPredict, by = 'pairCaType', 'pairwise')
contrast(emPredict, by = 'scrambleType', 'pairwise')

predictTestFlat <- predictTest %>%
  pivot_wider(names_from = c('pairCaType','scrambleType'), values_from = 'acc')

t.test(predictTestFlat$across_firstSame, mu = 0.5)
t.test(predictTestFlat$across_secondSame, mu = 0.5)
t.test(predictTestFlat$across_swapping, mu = 0.5)

t.test(predictTestFlat$within_firstSame, mu = 0.5)
t.test(predictTestFlat$within_secondSame, mu = 0.5)
t.test(predictTestFlat$within_swapping, mu = 0.5)
