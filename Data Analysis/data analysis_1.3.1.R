library(tidyverse)
library(reshape2)
library(lme4)
library(psycho)

#define the variables
coln_trans <- c('correctResponse','currSession','currTask','currTrial','pairCaType', 'pairSiType','picType','predictPair','predictType','response','responseTime','round','sessionNum','sizeType','stimulus','type')

# time check - how much time participants spent on the HIT
basicInfo <- !colnames(s) %in% coln_trans

basicInfo <- s[basicInfo] %>%
  filter(browserEnd == browserStart & engineStart == engineStart & languageStart == languageEnd & platformStart == platformEnd & widthStart == widthEnd) %>%
  mutate(timeOnExperiment = as.numeric(experimentTime)/60000)

view(basicInfo['timeOnExperiment'])
view(summary(basicInfo$timeOnExperiment))
ggplot(basicInfo,aes(x = timeOnExperiment)) + geom_histogram(binwidth = 5)


#split values in one cell to different rows
rawData <- s %>%
  separate_rows(all_of(coln_trans),sep = '\\|')

#sublet encoding session data
encodeData <- rawData %>%
  filter(currTask == 'encodePictures')
varNames <- c(coln_trans,'WorkerId')
encodeData <- encodeData[,varNames] %>%
  mutate(acc = ifelse(correctResponse == response,1,0)) %>%
  mutate(category = substr(stimulus,1,1)) %>%
  mutate(subCategory = substr(stimulus,1,2)) 

#aggregate data for each subcategory
accEncode1 <- encodeData %>%
  group_by(WorkerId,subCategory) %>%
  summarize(meanAccSubCategory = mean(acc))

ggplot(accEncode1,aes(x = subCategory, y = meanAccSubCategory,group = 1)) + geom_line() + facet_wrap(~WorkerId)

coverTask1 <- lmer(meanAccSubCategory~subCategory + (1|WorkerId), data = accEncode1)
summary(coverTask1) 
anova(coverTask1)

#check each image in each subcategory using BLUPS
#m <- glmer(correct ~ early_late_in_training + big_small + first_second + same_diff_cat + (1 | WorkerId) + (1 | item), family="binomial")f

#coef(m) #gives you the BLUPs

imModel <- glmer(acc~(1|WorkerId) + (1|stimulus), family = binomial, data = encodeData)
coefResult <- coef(imModel)
coefStimulus <- coefResult[[1]]
OutVals = boxplot(coefStimulus$`(Intercept)`)$out
outlierIndex <- which(coefStimulus$`(Intercept)` %in% OutVals)
outlierStimulus <- coefStimulus[outlierIndex,1]

#exclude rejected assignments
encodeData2 <- encodeData %>%
  filter(!WorkerId %in% scammerList)
imModel2 <- glmer(acc~(1|WorkerId) + (1|stimulus), family = binomial, data = encodeData2)
coefResult <- coef(imModel2)
coefStimulus <- coefResult[[1]]
coefStimulus$stimulus <- row.names(coefStimulus)
coefStimulus <- cbind(coefStimulus,subCategory = rep(c('a1','a2','a3','a4','b1','b2','b3','b4', 'c1', 'c2','c3', 'c4','d1','d2','d3','d4'),each = 30)) %>%
  group_by(subCategory)
OutVals = boxplot(coefStimulus$`(Intercept)`)$out
outlierIndex <- which(coefStimulus$`(Intercept)` %in% OutVals)
outlierStimulus <- coefStimulus[outlierIndex,] #c1_6,c1_20,c1_26, b4_7

#find outliers of each subcategory

coef<- coefStimulus %>%
  filter(subCategory == 'd4')
OutVals = boxplot(coef$`(Intercept)`)$out
outlierIndex <- which(coef$`(Intercept)` %in% OutVals)
outlierStimulus <- coef[outlierIndex,] #a1_6,b2_25,b2_6,

  
#get blups for item -- and check for items with low values (lower probability correct)
#Best Linear Unbiased Predictor --> similar to the fixed effect size but for random effects


# acc for each stimulus
stimulusAcc <- encodeData2 %>%
  group_by(stimulus,subCategory, category) %>%
  summarise(meanAcc = mean(acc))

stimulusList <- stimulusAcc %>%
  filter(meanAcc < 0.6)

#binomial test for each stimulus
binoTestData <- encodeData2 %>%
  group_by(stimulus) %>%
  summarise(correctTrials = sum(acc), totalTrials = n())
 
for(i in 1:nrow(binoTestData)){
  binoTestData$binoTestResult[i] <-binom.test(binoTestData$correctTrials[i], binoTestData$totalTrials[i], p = 0.5)$p.value
}

chanceStimulus <- binoTestData %>%
  filter(binoTestResult > 0.05) #number of trials is too small to compute a reliable test result.

#aggregate data across all category
accEncode2 <- encodeData %>%
  group_by(WorkerId) %>%
  summarize(meanAccCategory = mean(acc))
summary(accEncode2$meanAccCategory)
ggplot(accEncode2,aes(x = meanAccCategory)) + geom_histogram(binwidth = 0.1)
table(accEncode2$meanAccCategory) # 0.44 - 0.56 (6),0.65-0.77 (5), 0.8-0.99(16)

accEncode2 <-accEncode2 %>%
  mutate(accLevel = case_when(meanAccCategory < 0.56 ~ 'low', meanAccCategory < 0.8 & meanAccCategory > 0.6 ~'medium', meanAccCategory > 0.8 ~ 'high'))

#proportion of 'no response' 
accEncode3 <- encodeData %>%
  mutate(noResponse = ifelse(response == 'no response detected',1,0)) %>%
  group_by(WorkerId) %>%
  summarize(noResponse = mean(noResponse))

#encodeScammerCheck
encodeScammerCheck <- merge(accEncode2,accEncode3,by = 'WorkerId')
ggplot(encodeScammerCheck,aes(x = noResponse, y = meanAccCategory)) + geom_point()


#subset Math Data
mathData <- rawData %>%
  filter(currTask == 'mathDistract') %>%
  mutate(mathAcc = ifelse(response == correctResponse,1,0), mathNoResponse = ifelse(response == 'no response detected',1,0 )) 
mathData <- mathData[,c('WorkerId','mathAcc','response','responseTime','mathNoResponse')]

#calculate math acc and no_response rate
mathDistract <- mathData %>%
  group_by(WorkerId) %>%
  summarise(mathAcc = mean(mathAcc),mathNoResponse = mean(mathNoResponse))
ggplot(mathDistract,aes(x=mathAcc))+geom_histogram(binwidth = 0.05)

#scammercheck combining encode task and mathTask
scammerCheck <- merge(encodeScammerCheck, mathDistract, by = 'WorkerId')
view(filter(scammerCheck, mathAcc < 0.7))
view(filter(scammerCheck, accLevel != 'high'))

#potential scammer WorkerId
scammerList <-scammerCheck %>%
  filter(meanAccCategory < 0.6 & mathNoResponse > 0.8)


#subset memory test data 
memoryData <- rawData[,varNames] %>%
  filter(currTask == 'memoryTest') %>%
  mutate(acc = case_when(correctResponse == 'k' & response %in% c('j','k') ~ 1, correctResponse == 'k' & !response %in% c('j','k') ~ 0, correctResponse == 'd' & response %in% c('d','f') ~ 1, correctResponse == 'd' & !response %in% c('d','f') ~ 0))


#add quartile information from cover task
quartile <- encodeData[,c('WorkerId','currTrial','stimulus')] %>%
  group_by('WorkerId') %>%
  arrange('currTrial') %>%
  cbind(q = rep(1:4, times = 62, each = 64))
encodeData <- merge(encodeData, quartile[,c('WorkerId','q','stimulus')],by = c('WorkerId','stimulus'))
memoryData <- merge(memoryData,quartile[,c('WorkerId','q','stimulus')],by = c('WorkerId','stimulus'),all = TRUE)

#mark memory response as hit, miss, fa, cr
memoryTest <- memoryData %>%
  filter(response != 'no response detected') %>% #filter out no response trials
  mutate(sdt = case_when(picType == 'foil' & acc == 1 ~ 'cr', picType == 'foil' & acc == 0 ~ 'fa', picType == 'old' & acc == 1 ~ 'hit', picType == 'old' & acc == 0 ~ 'miss') ) 

#caculate d'
dprimeMemory <- memoryTest %>%
  group_by(WorkerId, type, predictType) %>%
  count(sdt) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n')
dprimeMemory[is.na(dprimeMemory)]<-0
indices <- psycho::dprime(dprimeMemory$n_hit,dprimeMemory$n_fa,dprimeMemory$n_miss,dprimeMemory$n_cr)

dMemoryTest1 <- cbind(dprimeMemory,indices)

#mixed-effect model
##filter out low quality data
includeList <- encodeScammerCheck %>%
  filter(meanAccCategory > 0.7 & noResponse < 0.1)
includeList <- includeList$WorkerId

dMemory1 <- dMemoryTest1 %>%
  filter(WorkerId %in% includeList)
dMemoryM1 <- lmer(dprime ~ type + (1|WorkerId),data = dMemory1)
anova(dMemoryM1)
ggplot(dMemory1, aes(x= type, y = aprime))+geom_boxplot()

dMemoryM2 <- lmer(dprime ~ type + predictType + (1|WorkerId),data = dMemory1)
anova(dMemoryM2)
ggplot(dMemory1, aes(x = type, y = aprime, color = predictType)) + geom_boxplot()

# hit_rate + fa_rate ? include 'no response answer'
hitMemoryTest1 <- memoryTest %>%
  group_by(WorkerId, type, predictType) %>%
  count(sdt) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n') %>%
  mutate(hit_rate = n_hit/(n_hit + n_miss),fa_rate = n_fa/(n_fa + n_cr))
#filter out low quality data
hitMemory1 <- filter(hitMemoryTest1, WorkerId %in% includeList)
hitMemoryModel1 <- lmer(hit_rate ~ type + (1|WorkerId), data = hitMemory1)
anova(hitMemoryModel1)


ggplot(hitMemory1,aes(x = type, y = hit_rate, color = predictType)) + geom_boxplot()
ggplot(hitMemory1,aes(x = type, y = fa_rate, color = predictType)) + geom_boxplot()

#temporal change + hit_rate  ## If include temporal factor, only hit_rate could be used.
hitMemoryTest2 <- memoryTest %>%
  group_by(WorkerId, type, predictType,q) %>%
  count(sdt) %>%
  pivot_wider(names_from = 'sdt', names_prefix = 'n_', values_from = 'n') %>%
  mutate(hit_rate = n_hit/(n_hit + n_miss))
#filter out low quality data
hitMemory2 <- filter(hitMemoryTest2, WorkerId %in% includeList)
ggplot(hitMemory2,aes(x = type, y = hit_rate, color = predictType)) + geom_boxplot()+facet_wrap(~q) #misleading because of excluded rows
##filter out na
hitMemory2Filter <- filter(hitMemoryTest2, !is.na(q)) %>%
  filter(is.na(hit_rate))
filterHitMemory2 <- filter(hitMemoryTest2,! WorkerId %in% hitMemory2Filter$WorkerId & WorkerId %in% includeList) ## filter out both na and low quality data
view(distinct(filterHitMemory2[,'WorkerId'])) ## only 8 participants
ggplot(filterHitMemory2,aes(x = type, y = hit_rate, color = predictType)) + geom_boxplot()+facet_wrap(~q)

     