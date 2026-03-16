memoryTest <- read_excel('data_organized.xlsx', sheet = 'memoryTest_general')
setwd('/Users/leichen/Research project/Predictive memory/Pilot data (2021.5.4-2021.5.5)')
library(tidyverse)
library(ggpubr)
library(rstatix)
hitrate <-memoryTest %>%
  gather(key = 'type',value = 'hit_rate',cue_old,filler_old,target_old) %>%
  convert_as_factor(sub,type)
hitrate %>%
  group_by(type) %>%
  get_summary_stats(hit_rate, type = "mean_sd")
bxp <- ggboxplot(hitrate, x = "type", y = "hit_rate", add = "point")
bxp
hitrate %>%
  group_by(type) %>%
  shapiro_test(hit_rate)
res.aov <- anova_test(data = hitrate, dv = hit_rate, wid = sub, within = type)
get_anova_table(res.aov)
pwc <- hitrate %>%
  pairwise_t_test(
    hit_rate ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
pwc <- pwc %>% add_xy_position(x = "type")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
hitrate <-memoryTest %>%
  gather(key = 'type',value = 'hit_rate',cue_cross,cue_within) %>%
  convert_as_factor(sub,type)
hitrate %>%
  group_by(type) %>%
  get_summary_stats(hit_rate, type = "mean_sd")
bxp <- ggboxplot(hitrate, x = "type", y = "hit_rate", add = "point")
bxp
hitrate %>%
  group_by(type) %>%
  shapiro_test(hit_rate)
res.aov <- anova_test(data = hitrate, dv = hit_rate, wid = sub, within = type)
get_anova_table(res.aov)
pwc <- hitrate %>%
  pairwise_t_test(
    hit_rate ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
pwc <- pwc %>% add_xy_position(x = "type")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
hitrate <-memoryTest %>%
  gather(key = 'type',value = 'hit_rate',target_cross,target_within) %>%
  convert_as_factor(sub,type)
hitrate %>%
  group_by(type) %>%
  get_summary_stats(hit_rate, type = "mean_sd")
bxp <- ggboxplot(hitrate, x = "type", y = "hit_rate", add = "point")
bxp
hitrate %>%
  group_by(type) %>%
  shapiro_test(hit_rate)
res.aov <- anova_test(data = hitrate, dv = hit_rate, wid = sub, within = type)
get_anova_table(res.aov)
pwc <- hitrate %>%
  pairwise_t_test(
    hit_rate ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
pwc <- pwc %>% add_xy_position(x = "type")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
hitrate <-memoryTest %>%
  gather(key = 'type',value = 'hit_rate',cue_cross, cue_within,filler_old,target_cross,target_within) %>%
  convert_as_factor(sub,type)
hitrate %>%
  group_by(type) %>%
  get_summary_stats(hit_rate, type = "mean_sd")
bxp <- ggboxplot(hitrate, x = "type", y = "hit_rate", add = "point")
bxp
hitrate %>%
  group_by(type) %>%
  shapiro_test(hit_rate)
res.aov <- anova_test(data = hitrate, dv = hit_rate, wid = sub, within = type)
get_anova_table(res.aov)
pwc <- hitrate %>%
  pairwise_t_test(
    hit_rate ~ type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
pwc <- pwc %>% add_xy_position(x = "type")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
