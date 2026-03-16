library(tidyverse)
library(reshape2)
library(lme4)
library(lmerTest)
library(psycho)
library(ggsignif)
library(emmeans)

#define the variables
coln_trans <- c('correctResponse','currSession','currTask','currTrial','pairCaType', 'pairSiType','picType','predictPair','predictType','response','responseTime','round','sessionNum','sizeType','stimulus','type')

# time check - how much time participants spent on the HIT
basicInfo <- !colnames(s) %in% coln_trans

basicInfo <- s[basicInfo] %>%
  filter(browserEnd == browserStart & engineStart == engineStart & languageStart == languageEnd & platformStart == platformEnd & widthStart == widthEnd) %>%
  mutate(timeOnExperiment = as.numeric(experimentTime)/60000)
ggplot(basicInfo,aes(x = timeOnExperiment)) + geom_histogram(binwidth = 5)


#split values in one cell to different rows
s <- s[s$AssignmentStatus != 'Rejected',]
rawData <- s%>%
  separate_rows(all_of(coln_trans), sep = '\\|')

################cover task##########################

#sublet encoding session data
encodeData <- rawData %>%
  filter(currTask == 'encodePictures')
varNames <- c(coln_trans,'WorkerId')
encodeData <- encodeData[,varNames] %>%
  mutate(acc = ifelse(correctResponse == response,1,0)) %>%
  mutate(category = substr(stimulus,1,1)) %>%
  mutate(subCategory = substr(stimulus,1,2)) 

#aggregate acc data for each subcategory
accEncode1 <- encodeData %>%
  group_by(WorkerId,subCategory) %>%
  summarize(meanAccSubCategory = mean(acc))
coverTask1 <- lmer(meanAccSubCategory~subCategory + (1|WorkerId), data = accEncode1)
summary(coverTask1) 
anova(coverTask1)
ggplot(accEncode1, aes(x = subCategory, y = meanAccSubCategory)) +stat_summary(fun = 'mean',geom = 'bar')

#aggregate acc data for each category
accEncode2 <- encodeData %>%
  group_by(WorkerId, category)%>%
  summarize(meanAccCategory = mean(acc))
coverTask2 <- lmer(meanAccCategory ~ category + (1|WorkerId), data = accEncode2)
summary(coverTask2)
anova(coverTask2)
ggplot(accEncode2, aes(x = category, y = meanAccCategory)) + stat_summary(fun = 'mean', geom = 'bar')

#aggregate acc data across all category
accEncode3 <- encodeData %>%
  group_by(WorkerId) %>%
  summarize(meanAcc = mean(acc))
summary(accEncode3$meanAcc)
ggplot(accEncode3,aes(x = meanAcc)) + geom_histogram(binwidth = 0.1)
table(accEncode3$meanAcc) 

#proportion of 'no response' 
accEncode4 <- encodeData %>%
  mutate(noResponse = ifelse(response == 'no response detected',1,0)) %>%
  group_by(WorkerId) %>%
  summarize(noResponse = mean(noResponse))

#merge acc and 'no response'
encodeScammerCheck <- merge(accEncode3,accEncode4,by = 'WorkerId')
ggplot(encodeScammerCheck,aes(x = noResponse, y = meanAcc)) + geom_point()

#includeList made based on cover task acc and 'no response' rate
includeList <- encodeScammerCheck %>%
  filter(meanAcc >= 0.7)
includeWorker <- includeList$WorkerId



################### Memory Test ###########################

#subset memory test data 
memoryData <- rawData[,varNames] %>%
  filter(currTask == 'memoryTest') %>%
  mutate(acc = case_when(correctResponse == 'k' & response %in% c('j','k') ~ 1, correctResponse == 'k' & !response %in% c('j','k') ~ 0, correctResponse == 'd' & response %in% c('d','f') ~ 1, correctResponse == 'd' & !response %in% c('d','f') ~ 0))


#add quartile information from cover task
quartile <- encodeData[,c('WorkerId','currTrial','stimulus')] %>%
  group_by('WorkerId') %>%
  arrange('currTrial') %>%
  cbind(q = rep(1:4, times = nrow(s), each = 64))
encodeData <- merge(encodeData, quartile[,c('WorkerId','q','stimulus')],by = c('WorkerId','stimulus'))
memoryData <- merge(memoryData,quartile[,c('WorkerId','q','stimulus')],by = c('WorkerId','stimulus'),all = TRUE)

#mark memory response as hit, miss, fa, cr
memoryTest <- memoryData %>%
  filter(response != 'no response detected') %>% #filter out no response trials
  mutate(sdt = case_when(picType == 'foil' & acc == 1 ~ 'cr', picType == 'foil' & acc == 0 ~ 'fa', picType == 'old' & acc == 1 ~ 'hit', picType == 'old' & acc == 0 ~ 'miss') )

memoryCount <- memoryTest %>%
  group_by(WorkerId, type, predictType) %>%
  count(sdt) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n')
memoryCount[is.na(memoryCount)] <- 0

#calculate precision
memoryPrecision <- memoryCount %>%
  mutate(precision = n_hit/(n_hit + n_fa)) %>%
  filter(WorkerId %in% includeWorker)
memoryPrecisionModel1 <- lmer(precision ~ type + (1|WorkerId), data = memoryPrecision)
anova(memoryPrecisionModel1)
summary(memoryPrecisionModel1)
memoryPrecisionSEM <- memoryPrecision %>%
  group_by(type, predictType) %>%
  summarize(meanPrecision = mean(precision), se = sd(precision)/sqrt(length(precision)))
ggplot(memoryPrecision, aes(x= type, y = precision, color = predictType)) + geom_boxplot() #+ geom_signif(y_position = c(0.85,0.85), xmin = c(1,2), xmax = c(2,3), annotations = c('p = 0.16', 'p = 0.29'), tip_length = 0.01)
em <- emmeans::emmeans(memoryPrecisionModel1,'type')
emmeans::contrast(em, method = "pairwise")

ggplot(memoryPrecisionSEM, aes(x = type, y = meanPrecision, fill = predictType)) + geom_bar(position=position_dodge(),stat = "identity") + geom_errorbar(aes(x = type, ymin = meanPrecision-se, ymax = meanPrecision + se), position=position_dodge(0.9), width=0.4, colour="black", alpha=0.9, size=0.8) + theme_classic()+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + geom_signif(y_position = c(0.72,0.72), xmin = c(1,2), xmax = c(2,3), annotations = c('p = 0.16', 'p = 0.29'), tip_length = 0.1)

#calculate sensitivity/hit_rate
memorySensitivity <- memoryCount %>%
  mutate(sensitivity = n_hit/(n_hit + n_miss)) %>%
  filter(WorkerId %in% includeWorker)
memoryHitRateSEM <- memorySensitivity %>%
  group_by(type, predictType) %>%
  summarize(hitRateMean = mean(sensitivity), se = sd(sensitivity)/sqrt(length(sensitivity)))

memoryHitRateModel1 <- lmer(sensitivity ~ type + (1|WorkerId), data = memorySensitivity)
anova(memoryHitRateModel1)
summary(memoryHitRateModel1)

memoryHitRateCompare <- memorySensitivity %>%
  filter(type != 'filler')
memoryHitRateFiller <- memorySensitivity %>%
  filter(type == 'filler')
memoryHitRateModel2 <- lmer(sensitivity ~ type + predictType + type * predictType + (1|WorkerId), data = memoryHitRateCompare)
anova(memoryHitRateModel2)
summary(memoryHitRateModel2) ## type p = 0.23, type*predictType p = 0.12

memoryHitRateWide <- memoryHitRateCompare %>%
  pivot_wider(id_cols = c(WorkerId, type, predictType, sensitivity), names_from = c(type, predictType), values_from = sensitivity) %>%
  cbind(filler = memoryHitRateFiller$sensitivity)
t.test(memoryHitRateWide$cue_within, memoryHitRateWide$target_within, paired = TRUE, cof.level = 0.95) ## p = 0.04
t.test(memoryHitRateWide$cue_within, memoryHitRateWide$filler, paired = TRUE, cof.level = 0.95) ## p = 0.25
t.test(memoryHitRateWide$cue_cross, memoryHitRateWide$filler, paired = TRUE, cof.level = 0.95) 
t.test(memoryHitRateWide$target_cross, memoryHitRateWide$filler, paired = TRUE, cof.level = 0.95)## p = 0.25

ggplot(memorySensitivity, aes(x = type, y = sensitivity, color = predictType)) + geom_boxplot()
ggplot(memoryHitRateSEM, aes(x = type, y = hitRateMean, fill = predictType)) + geom_bar(position=position_dodge(),stat = "identity") + geom_errorbar(aes(x = type, ymin = hitRateMean-se, ymax = hitRateMean + se), position=position_dodge(0.9), width=0.4, colour="black", alpha=0.9, size=0.8) + geom_signif( y_position = c(0.7,0.75, 0.8), xmin = c(1.2, 0.8, 1.2), xmax = c(3.2, 2,2), annotations = c('*','NS', 'p = 0.15')) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

#calculate dprime
indices <- psycho::dprime(memoryCount$n_hit,memoryCount$n_fa,memoryCount$n_miss,memoryCount$n_cr)
dprimeMemory <- cbind(memoryCount, indices) %>%
  filter(WorkerId %in% includeWorker)
dprimeMemorySEM <- dprimeMemory %>%
  group_by(type, predictType) %>%
  summarize(dprimeMean = mean(dprime), sedprime = sd(dprime)/sqrt(length(dprime)),aprimeMean = mean(aprime), seaprime = sd(aprime)/sqrt(length(aprime)))
ggplot(dprimeMemory, aes(x = type, y = dprime, color = predictType)) + geom_boxplot()
aprimeModel1 <- lmer(aprime ~ type + (1|WorkerId), data = dprimeMemory)
anova(aprimeModel1)
em <- emmeans::emmeans(aprimeModel1,'type')
emmeans::contrast(em, method = 'pairwise')

ggplot(dprimeMemorySEM, aes(x = type, y = dprimeMean, fill = predictType)) + geom_bar(position=position_dodge(),stat = "identity") + geom_errorbar(aes(x = type, ymin = dprimeMean-sedprime, ymax = dprimeMean + sedprime), position=position_dodge(0.9), width=0.4, colour="black", alpha=0.9, size=0.8)
ggplot(dprimeMemory, aes(x = type, y = aprime, color = predictType)) + geom_boxplot()
ggplot(dprimeMemorySEM, aes(x = type, y = aprimeMean, fill = predictType)) + geom_bar(position=position_dodge(),stat = "identity") + geom_errorbar(aes(x = type, ymin = aprimeMean-seaprime, ymax = aprimeMean + seaprime), position=position_dodge(0.9), width=0.4, colour="black", alpha=0.9, size=0.8)+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) #+ geom_signif(y_position = c(0.65, 0.65),xmin = c(1,2.02),xmax = c(1.98,3),annotations = c('p = 0.13', 'p = 0.057'))

#calculate bias 
dprimeMemory <- dprimeMemory %>%
  drop_na()
biasPlot <- dprimeMemory %>%
  group_by(type, predictType) %>%
  summarise(betaMean = mean(beta),seBeta = sd(beta)/sqrt(length(beta)),bppdMean = mean(bppd), sebppd = sd(bppd)/sqrt(length(bppd)), cMean = mean(c), seC = sd(c)/sqrt(length(c)))
ggplot(biasPlot, aes(x = type, y = betaMean, fill = predictType)) + geom_bar(position = position_dodge(), stat = 'identity') + geom_errorbar(aes(x = type, ymin = betaMean - seBeta, ymax = betaMean + seBeta), position = position_dodge(0.9), width = 0.4, color = 'black', alpha = 0.9, size = 0.8) + scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
betaModel1 <- lmer(beta ~ type + (1|WorkerId), data = dprimeMemory)
anova(betaModel1)
dprimeMemoryCompare <- dprimeMemory %>%
  filter(type != 'filler')
betaModel2 <- lmer(beta ~ type * predictType + (1|WorkerId), data = dprimeMemoryCompare)
anova(betaModel2)
dprimeMemoryWide <- dprimeMemory %>%
  pivot_wider(id_cols = c(WorkerId, type, predictType, beta,bppd,c), names_from = c('type','predictType'),values_from = c(beta,bppd,c))
biasWithCue <- dprimeMemory %>%
  filter(type == 'cue' & predictType == 'within')
biasCrossTarget <- dprimeMemory %>%
  filter(type == 'target',predictType == 'cross')
biasFiller <- dprimeMemory %>%
  filter(type == 'filler')
t.test(biasWithCue$beta, mu = 1)
t.test(biasCrossTarget$beta, mu = 1)
biasWithTarget <- dprimeMemory %>%
  filter(type == 'target' & predictType == 'within')
t.test(biasWithTarget$beta, mu = 1)
t.test(dprimeMemoryWide$beta_cue_within,dprimeMemoryWide$beta_filler_NA,paired = TRUE,cof.level = 0.05)
t.test(dprimeMemoryWide$beta_cue_within, dprimeMemoryWide$beta_target_within,paired = TRUE, cof.level = 0.05)

ggplot(biasPlot, aes(x = type, y = bppdMean, fill = predictType)) + geom_bar(position = position_dodge(), stat = 'identity') + geom_errorbar(aes(x = type, ymin = bppdMean - sebppd, ymax = bppdMean + sebppd), position = position_dodge(0.9), width = 0.4, color = 'black', alpha = 0.9, size = 0.8) + scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
t.test(biasWithCue$bppd, mu = 0)
t.test(biasWithTarget$bppd, mu = 0)
t.test(biasFiller$bppd, mu = 0)
t.test(biasCrossTarget$bppd, mu = 0)
bppdModel1 <- lmer(bppd~type + (1|WorkerId), data = dprimeMemory)
anova(bppdModel1)
bppdModel2 <- lmer(bppd ~ type * predictType + (1|WorkerId), data = dprimeMemoryCompare)
anova(bppdModel2)
t.test(dprimeMemoryWide$bppd_cue_cross, dprimeMemoryWide$bppd_target_cross, paied = TRUE, cof.level = 0.05)
t.test(dprimeMemoryWide$bppd_cue_within, dprimeMemoryWide$bppd_target_within, paied = TRUE, cof.level = 0.05)
t.test(dprimeMemoryWide$bppd_cue_within, dprimeMemoryWide$bppd_filler_NA, paired = TRUE, cof.level = 0.05)
t.test(dprimeMemoryWide$bppd_filler_NA, dprimeMemoryWide$bppd_target_within, paired = TRUE, cof.level = 0.05)

ggplot(biasPlot, aes(x = type, y = cMean, fill = predictType)) + geom_bar(position = position_dodge(), stat = 'identity') + geom_errorbar(aes(x = type, ymin = cMean - seC, ymax = cMean + seC), position = position_dodge(0.9), width = 0.4, color = 'black', alpha = 0.9, size = 0.8) + scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
t.test(biasWithCue$c, mu = 0)
t.test(biasCrossTarget$c, mu = 0)
t.test(biasWithTarget$c, mu = 0)
cModel1 <- lmer(c~type + (1|WorkerId), data = dprimeMemory)
anova(cModel1)
cModel2 <- lmer(c~type*predictType + (1|WorkerId), data = dprimeMemoryCompare)
anova(cModel2)
t.test(dprimeMemoryWide$c_cue_within, dprimeMemoryWide$c_target_within, paired = TRUE, cof.level = 0.05)
t.test(dprimeMemoryWide$c_cue_within, dprimeMemoryWide$c_filler_NA, paired = TRUE, cof.level = 0.05)
t.test(dprimeMemoryWide$c_target_within, dprimeMemoryWide$c_filler_NA, paired = TRUE, cof.level = 0.05)
t.test(dprimeMemoryWide$c_target_cross, dprimeMemoryWide$c_filler_NA, paired = TRUE, cof.level = 0.05)

#calculate acc
memoryAcc <- memoryData %>%
  group_by(WorkerId, type, predictType) %>%
  summarize(memoryAcc = mean(acc)) %>%
  filter(WorkerId %in% includeWorker)
meanAccSEM <- memoryAcc %>%
  group_by(type, predictType) %>%
  summarize(meanAccuracy = mean(memoryAcc), se = sd(memoryAcc)/sqrt(length(memoryAcc)))
ggplot(memoryAcc, aes(x = type, y = memoryAcc, color = predictType)) + geom_boxplot()
ggplot(meanAccSEM, aes(x = type, y = meanAccuracy, fill = predictType)) + geom_bar(position=position_dodge(),stat = "identity") + geom_errorbar(aes(x = type, ymin = meanAccuracy-se, ymax = meanAccuracy + se), position=position_dodge(0.9), width=0.4, colour="black", alpha=0.9, size=0.8)

#calculate false alarm rate
memoryFaRate <- memoryCount %>%
  mutate(fa_rate = n_fa/(n_cr + n_fa)) %>%
  filter(WorkerId %in% includeWorker)
memoryFaRateSEM <- memoryFaRate %>%
  group_by(type, predictType) %>%
  summarize(meanFaRate = mean(fa_rate), se = sd(fa_rate)/sqrt(length(fa_rate)))

memoryFaRateModel1 <- lmer(fa_rate ~ type + (1|WorkerId), data = memoryFaRate)
anova(memoryFaRateModel1)
summary(memoryFaRateModel1) 

memoryFaRateCompare <- memoryFaRate %>%
  filter(type != 'filler')
memoryFaRateFiller <- filter(memoryFaRate, type == 'filler')
memoryFaRateModel2 <- lmer(fa_rate ~ type + predictType + type*predictType + (1|WorkerId), data = memoryFaRateCompare)
anova(memoryFaRateModel2)
summary(memoryFaRateModel2)

memoryFaRateWide <- memoryFaRateCompare %>%
  pivot_wider(id_cols = c(WorkerId, type, predictType, fa_rate), names_from = c(type,predictType), values_from = fa_rate) %>%
  cbind(filler = memoryFaRateFiller$fa_rate)

t.test(memoryFaRateWide$cue_within, memoryFaRateWide$target_within,paired = TRUE, cof.level = 0.95) ## p = 0.07
t.test(memoryFaRateWide$cue_cross,memoryFaRateWide$target_cross, paired = TRUE, cof.level = 0.95)
t.test(memoryFaRateWide$cue_cross, memoryFaRateWide$filler, paired = TRUE, cof.level = 0.95)
t.test(memoryFaRateWide$cue_within, memoryFaRateWide$filler,paired = TRUE, cof.level = 0.95)## p = 0.21
t.test(memoryFaRateWide$target_within, memoryFaRateWide$filler, paired = TRUE, cof.level = 0.95)
t.test(memoryFaRateWide$target_cross, memoryFaRateWide$filler, paired = TRUE, cof.level = 0.95)
t.test(memoryFaRateWide$cue_within,memoryFaRateWide$cue_cross, paired = TRUE, cof.level = 0.95)



ggplot(memoryFaRate, aes(x = type, y = fa_rate, fill = predictType)) + geom_boxplot()
ggplot(memoryFaRateSEM, aes(x = type, y = meanFaRate, fill = predictType)) + geom_bar(position=position_dodge(),stat = "identity") + geom_errorbar(aes(x = type, ymin = meanFaRate-se, ymax = meanFaRate + se), position=position_dodge(0.9), width=0.4, colour="black", alpha=0.9, size=0.8) + geom_signif(y_position = c(0.64, 0.68, 0.72, 0.76), xmin = c(0.75, 1.25, 2, 2), xmax = c(2,2,2.75, 3.2), annotations = c('NS','p = 0.104','NS','NS')) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

#######transform responses into familiarity scale###########
memoryTestFamiliarity <- memoryData %>%
  filter(response != 'no response detected')%>%
  mutate(familiarity = case_when(correctResponse == 'd'& picType == 'foil'& response == 'd'~1,correctResponse == 'd'& picType == 'foil'&response == 'f'~2,correctResponse == 'd'& picType == 'foil'&response == 'j'~3,correctResponse == 'd'& picType == 'foil'&response == 'k'~ 4,correctResponse == 'd'& picType == 'old'& response == 'd'~4,correctResponse == 'd'& picType == 'old'&response == 'f'~3,correctResponse == 'd'& picType == 'old'&response == 'j'~2,correctResponse == 'd'& picType == 'old'&response == 'k'~ 1,correctResponse == 'k'& picType == 'foil'& response == 'd'~4,correctResponse == 'k'& picType == 'foil'&response == 'f'~3,correctResponse == 'k'& picType == 'foil'&response == 'j'~2,correctResponse == 'k'& picType == 'foil'&response == 'k'~ 1,correctResponse == 'k'& picType == 'old'& response == 'd'~1,correctResponse == 'k'& picType == 'old'&response == 'f'~2,correctResponse == 'k'& picType == 'old'&response == 'j'~3,correctResponse == 'k'& picType == 'old'&response == 'k'~ 4)) %>%
  filter(WorkerId %in% includeWorker) %>%
  group_by(WorkerId, picType,type,predictType) %>%
  summarise(meanFamiliarity = mean(familiarity))
memoryFamiliarityModel1 <- lmer(meanFamiliarity ~ picType * type +(1|WorkerId), data = memoryTestFamiliarity) 
anova(memoryFamiliarityModel1)
summary(memoryFamiliarityModel1)
ggplot(memoryTestFamiliarity, aes(y = meanFamiliarity, x = type, fill = predictType)) + geom_boxplot() + facet_wrap(~picType)

################## Cover task - Prediction #####################

coverTaskAccType <- encodeData %>%
  group_by(WorkerId, type) %>%
  summarize(acc = mean(acc)) %>%
  filter(WorkerId %in% includeWorker)
coverTaskAccTypeModel1 <- lmer(acc ~ type + (1|WorkerId), data = coverTaskAccType)
anova(coverTaskAccTypeModel1)
summary(coverTaskAccTypeModel1)
ggplot(coverTaskAccType, aes(x = type, y = acc)) + geom_boxplot()

coverTaskAccTypeTime <- encodeData %>%
  mutate(q = factor(q, levels = c(1,2,3,4))) %>%
  group_by(WorkerId, type, q) %>%
  summarise(acc = mean(acc)) %>%
  filter(WorkerId %in% includeWorker)
coverTaskAccTypeModel2<- lmer(acc ~ type + q + type*q + (1|WorkerId), data = coverTaskAccTypeTime)
anova(coverTaskAccTypeModel2)
summary(coverTaskAccTypeModel2)
coverTaskAccTypeTimePlot <- coverTaskAccTypeTime %>%
  group_by(type, q) %>%
  summarise(acc = mean(acc))
ggplot(coverTaskAccTypeTimePlot, aes(x = factor(q), y = acc, color = type, group = type)) + geom_point() + geom_line()

coverTaskAccPredictType <- encodeData %>%
  group_by(WorkerId, type, predictType) %>%
  summarize(acc = mean(acc)) %>%
  filter(WorkerId %in% includeWorker & type != 'filler')
coverTaskAccPredictTypeModel1 <- lmer(acc~type + predictType + type * predictType + (1|WorkerId), data = coverTaskAccPredictType)
anova(coverTaskAccPredictTypeModel1)
summary(coverTaskAccPredictTypeModel1)
ggplot(coverTaskAccPredictType, aes(x = type, y = acc, color = predictType)) + geom_boxplot()

coverTaskAccPredictTypeTime <- encodeData %>%
  group_by(WorkerId, type, predictType,q) %>%
  summarise(acc = mean(acc))%>%
  filter(WorkerId %in% includeWorker & type != 'filler')
coverTaskAccPredictTypeModel2 <- lmer(acc ~ type + predictType + q + (1|WorkerId), data = coverTaskAccPredictTypeTime)
anova(coverTaskAccPredictTypeModel2)
summary(coverTaskAccPredictTypeModel2)

coverTaskAccGeneralPlot <- encodeData %>%
  group_by(WorkerId, type, predictType,q) %>%
  summarise(acc = mean(acc))%>%
  filter(WorkerId %in% includeWorker) %>%
  pivot_wider(names_from = c(type, predictType), values_from = acc) %>%
  pivot_longer(cols = c(cue_cross, cue_within, target_cross, target_within, filler_NA), names_to = 'combo', values_to = 'acc') %>%
  ungroup() %>%
  group_by(combo, q) %>%
  summarise(acc = mean(acc))
ggplot(coverTaskAccGeneralPlot, aes(x = factor(q), y = acc, group = combo, color = combo)) + geom_point() + geom_line()

coverTaskRTType <- encodeData %>%
  filter(WorkerId %in% includeWorker & response == correctResponse)%>%
  group_by(WorkerId, type) %>%
  mutate(rt = as.numeric(responseTime)) %>%
  summarize(rt = median(rt)) 
coverTaskRTTypeModel1 <- lmer(rt~type+(1|WorkerId), data = coverTaskRTType)
anova(coverTaskRTTypeModel1)
summary(coverTaskRTTypeModel1)
ggplot(coverTaskRTType, aes(x = type, y = rt, group = type)) + geom_boxplot()+geom_signif(y_position = 920,xmin = 1,xmax = 3, annotations = c('p = 0.11'), tip_length = 0.01) 

coverTaskRTTypeTime <- encodeData %>%
  filter(WorkerId %in% includeWorker & response == correctResponse)%>%
  mutate(rt = as.numeric(responseTime), q = factor(q, levels = c(1,2,3,4)))%>%
  group_by(WorkerId, type, q)%>%
  summarise(rt = median(rt))
coverTaskRTTypeModel2 <- lmer(rt ~ type + q + type * q + (1|WorkerId), data = coverTaskRTTypeTime)
anova(coverTaskRTTypeModel2)
summary(coverTaskRTTypeModel2)
coverTaskRTTypeTimePlot <- coverTaskRTTypeTime %>%
  group_by(type, q) %>%
  summarise(rt = mean(rt))
ggplot(coverTaskRTTypeTimePlot, aes(x = q, y = rt, color = type, group = type)) + geom_point() + geom_line()

coverTaskRTPredictType <- encodeData %>%
  filter(WorkerId %in% includeWorker & response == correctResponse & type != 'filler') %>%
  group_by(WorkerId, type, predictType) %>%
  mutate(rt = as.numeric(responseTime)) %>%
  summarise(rt = median(rt))
coverTaskRTPredictTypeModel1<- lmer(rt ~ type * predictType + (1|WorkerId), data = coverTaskRTPredictType)
anova(coverTaskRTPredictTypeModel1)
summary(coverTaskRTPredictTypeModel1)
ggplot(coverTaskRTPredictType,aes(x = type, color = predictType, y = rt)) + geom_boxplot()+ geom_signif(y_position = 950,xmin = 1,xmax = 2, annotations = c('NS'), tip_length = 0.01) 

coverTaskRTPredictTypeTime <- encodeData %>%
  filter(WorkerId %in% includeWorker & response == correctResponse & type != 'filler') %>%
  mutate(rt = as.numeric(responseTime), q = factor(q, levels = c(1,2,3,4))) %>%
  group_by(WorkerId, type, predictType, q) %>%
  summarise(rt = median(rt))
coverTaskRTPredictTypeModel2 <-lmer(rt ~ type * predictType * q + (1|WorkerId), data = coverTaskRTPredictTypeTime)
anova(coverTaskRTPredictTypeModel2)
summary(coverTaskRTPredictTypeModel2)
coverTaskRTPredictTypeGeneralPlot <- encodeData %>%
  filter(WorkerId %in% includeWorker & response == correctResponse) %>%
  mutate(rt = as.numeric(responseTime)) %>%
  group_by(WorkerId, type, predictType, q) %>%
  summarise(rt = median(rt)) %>%
  pivot_wider(names_from = c(type, predictType), values_from = rt) %>%
  pivot_longer(cols = c(cue_within, cue_cross, target_within, target_cross, filler_NA), names_to = 'combo', values_to = 'rt') %>%
  ungroup() %>%
  filter(combo != 'filler_NA') %>%
  group_by(combo, q)%>%
  summarise(rt = mean(rt))
ggplot(coverTaskRTPredictTypeGeneralPlot, aes(x = q, y = rt, group = combo, color = combo)) + geom_point() + geom_line() 

################## Prediction test ########################
#Subset prediction data
predictData <- rawData[,varNames] %>%
  filter(currTask == 'predictTest' & correctResponse != 'no correct answer' & response != 'no response detected') %>%
  mutate(acc = ifelse(response == correctResponse,1,0))
predictResult <- predictData %>%
  group_by(WorkerId, pairCaType) %>%
  summarise(meanAcc = mean(acc)) %>%
  filter(WorkerId %in% includeWorker)
predictResultSEM <- predictResult %>%
  group_by(pairCaType) %>%
  summarise(meanAccuracy = mean(meanAcc), se = sd(meanAcc)/sqrt(length(meanAcc)))
ggplot(predictResultSEM, aes(x = pairCaType, y = meanAccuracy)) + geom_bar(position=position_dodge(),stat = "identity") + geom_errorbar(aes(x = pairCaType, ymin = meanAccuracy - se, ymax = meanAccuracy + se),position=position_dodge(0.9), width=0.4, colour="black", alpha=0.9, size=0.8) + geom_hline(yintercept = 0.5, linetype="longdash") #+ #geom_text(x = 2.1, y = 0.63, label = '> 0.5 *')
predictResultTest <- predictResult %>%
  pivot_wider(names_from = pairCaType, values_from = meanAcc)
t.test(predictResultTest$cross,predictResultTest$within, paired = TRUE, conf.level = 0.95)
t.test(predictResultTest$cross, mu = 0.5)
t.test(predictResultTest$within, mu = 0.5)


#################### Cover task - semantic similarity score #################
coverTaskData <- encodeData %>%
  mutate(currTrial = as.integer(encodeData$currTrial)) %>%
  arrange(WorkerId,currTrial)
for(par in 1: length(unique(coverTaskData$WorkerId))){
  for (trial in 1:256){
    if(trial == 256){
      next
    }
    rowIndex <- 256 *(par -1) + trial
    coverTaskData[rowIndex, 22] <-semanticSimilarity[coverTaskData$subCategory[rowIndex],coverTaskData$subCategory[rowIndex + 1]]
  }
}
colnames(coverTaskData)[22] <- 'nextSemanticSimilarity'

for(par in 1: length(unique(coverTaskData$WorkerId))){
  for (trial in 1:256){
    if(trial == 1){
      next
    }
    rowIndex <- 256 *(par -1) + trial
    coverTaskData[rowIndex, 23] <-semanticSimilarity[coverTaskData$subCategory[rowIndex],coverTaskData$subCategory[rowIndex - 1]]
  }
}
colnames(coverTaskData)[23] <-'priorSemanticSimilarity'

############## Compare within and across category statistical learning ################


############ calculate SLP by log transformation ############
logTrans <- encodeData %>%
  mutate(rt = case_when(acc == 1 ~ as.numeric(responseTime),acc == 0 ~ 0),trialNo = as.numeric(currTrial), logTime = log10(rt))
diff <- logTrans %>%
  group_by(WorkerId) %>%
  arrange(trialNo, .by_group = TRUE) %>%
  mutate(diff = logTime - lead(logTime, default = NA))
##filter out NA trials##
SLP <- diff %>%
  mutate(diffType = case_when(type == 'cue' ~ 'cue-target', type == 'target' & lead(type) == 'filler' ~ 'target-filler', type == 'target' & lead(type) == 'cue' ~ 'target-cue', type == 'filler' & lead(type) == 'filler' ~ 'filler-filler', type == 'filler' & lead(type) == 'cue' ~ 'filler-cue')) %>%
  mutate(diffCa = case_when(category == lead(category) ~ 'withinCa', category != lead(category) ~ 'acrossCa'))%>%
  filter(!is.na(diff) & diff != -Inf & diff != Inf)

########## t test ###############
meanSLP <- SLP %>%
  group_by(WorkerId, q,diffType, diffCa) %>%
  summarise(meanSLP = mean(diff))

## cue-target vs 0 ##
pairCompare <- meanSLP %>%
  filter(WorkerId %in% includeWorker) %>%
  group_by(WorkerId, diffType) %>%#exclude workers
  summarise(mean = mean(meanSLP))

cueTarget <- filter(pairCompare, diffType == 'cue-target')
t.test(cueTarget$mean,mu = 0 ) #significantly larger than 0

## target-cue vs 0 ##
targetCue <- filter(pairCompare, diffType == 'target-cue')
t.test(targetCue$mean, mu = 0) # nearly significantly smaller than 0

## target-filler vs 0 ##
targetFiller <- filter(pairCompare, diffType == 'target-filler')
t.test(targetFiller$mean, mu = 0) # no difference

## filler-filler vs 0 ##
fillerFiller <- filter(pairCompare, diffType == 'filler-filler')
t.test(fillerFiller$mean, mu = 0) # no difference

## filler-cue vs 0 ##
fillerCue <- filter(pairCompare, diffType == 'filler-cue')
t.test(fillerCue$mean, mu = 0) # no difference

##### plot pair comparison #####
ggplot(pairCompare, aes(x = diffType, y = mean)) + geom_boxplot() + geom_jitter(color = 'black', size = 0.4, alpha = 0.9)

#### Compare within vs across category cue-target SLP ####
categoryCompare <- meanSLP %>%
  filter(WorkerId %in% includeWorker & diffType == 'cue-target') %>%
  group_by(WorkerId, diffCa) %>%
  summarise(mean = mean(meanSLP))%>%
  pivot_wider(names_from = diffCa, values_from = mean)

t.test(categoryCompare$acrossCa, categoryCompare$withinCa, paired = TRUE, cof.level = 0.95)  # t = -1.66, p = 0.1 acrossCa smaller than withCa

######### mixed-effect regression including within vs across, cue-target vs filler-filler, quartiles ###########
cueTargetPairs <- meanSLP %>%
  filter(diffType == 'cue-target'| diffType == 'filler-filler')
regressionModel <- lmer(meanSLP ~ q * diffType * diffCa + (1|WorkerId), data = cueTargetPairs)
anova(regressionModel)
summary(regressionModel)

