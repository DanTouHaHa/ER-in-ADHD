setwd("E:/Fudan_Luoqiang_ADHD_Project/20220923_ABCD数据库信息/20221111_DataAnalysis/影像数据")
library(readxl)
ABCD_T1_ECM_uncorrtced <- read_excel("ABCD_T1_ECM_uncorrtced.xlsx")

library(lmerTest)
library(tidyverse)
library(effects)
library(effectsize)
library(simr)
######如果涉及到多组被试或者交叉项，需要 emmeans包进行事后检验与简单效应分析
library(emmeans)
library(blme)
#section 数据清洗----
###### 认知、情绪、动机、症状区分维度，先都按照不区分维度进行计算。
ABCD_T1_ECM_uncorrtced$TotalCognition <- rowSums(ABCD_T1_ECM_uncorrtced[,c(6,7,8,11,12,13,25)],na.rm = T)
ABCD_T1_ECM_uncorrtced$Memory <- rowSums(ABCD_T1_ECM_uncorrtced[,c(12,25)], na.rm = T)
ABCD_T1_ECM_uncorrtced$EF <- rowSums(ABCD_T1_ECM_uncorrtced[,c(8,11)])
ABCD_T1_ECM_uncorrtced$Verbal <- rowSums(ABCD_T1_ECM_uncorrtced[,c(7,13)],na.rm = T)
ABCD_T1_ECM_uncorrtced$Spatial <- rowSums(ABCD_T1_ECM_uncorrtced[,c(6)],na.rm = T)

ABCD_T1_ECM_uncorrtced$BISBAS <- rowSums(ABCD_T1_ECM_uncorrtced[,c(26:45)],na.rm = T)
ABCD_T1_ECM_uncorrtced$BIS <- rowSums(ABCD_T1_ECM_uncorrtced[,c(26:32)],na.rm = T)
ABCD_T1_ECM_uncorrtced$BAS <- rowSums(ABCD_T1_ECM_uncorrtced[,c(33:45)],na.rm = T)
ABCD_T1_ECM_uncorrtced$BAS_reward <- rowSums(ABCD_T1_ECM_uncorrtced[,c(33:37)],na.rm = T)
ABCD_T1_ECM_uncorrtced$BAS_drive <- rowSums(ABCD_T1_ECM_uncorrtced[,c(38:41)],na.rm = T)
ABCD_T1_ECM_uncorrtced$BAS_funseeking <- rowSums(ABCD_T1_ECM_uncorrtced[,c(42:45)],na.rm = T)
###### T1-LMM模型, 5121个被试
str(ABCD_T1_ECM_uncorrtced)
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)
del1 <- which(ABCD_T1_ECM_uncorrtced$rawscore == "NA")
del2 <- which(ABCD_T1_ECM_uncorrtced$tscore == "NA")
del3 <- which(ABCD_T1_ECM_uncorrtced$persistent20230220 == "NA")
#ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del3,]
ABCD_T1_ECM_uncorrtced[554] <- lapply(ABCD_T1_ECM_uncorrtced[554], as.numeric)

#section ADHD症状----
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)

#####敏感性分析需要额外控制的协变量
BMI <- read_excel("BMI匹配完被试.xlsx")
ABCD_T1_ECM_uncorrtced <- left_join(ABCD_T1_ECM_uncorrtced, BMI,  by="src_subject_id")
ABCD_T1_ECM_uncorrtced[574] <- lapply(ABCD_T1_ECM_uncorrtced[574], as.numeric)
Puberty <- read_excel("Puberty.xlsx")
ABCD_T1_ECM_uncorrtced <- left_join(ABCD_T1_ECM_uncorrtced, Puberty,  by="src_subject_id")
ABCD_T1_ECM_uncorrtced[575:577] <- lapply(ABCD_T1_ECM_uncorrtced[575:577], as.numeric)

##### ADHD症状的神经相关性---控制ICV -----
##### area，，，，，敏感性分析额外控制BMI puberty
result_area_symptom <- c()
for (i in 287:354){
  fit <- lmer(rawscore ~ ABCD_T1_ECM_uncorrtced[,i] +
                sex2 + Age2 + demo_comb_income_v2 +  demo_prnt_ed_v2 + 
                as.factor(race_ethnicity) +  smri_vol_scs_intracranialv +
                (1|site_id_l/rel_family_id),data = ABCD_T1_ECM_uncorrtced)
  result_area_symptom <- rbind(result_area_symptom,
                               c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,4,5)])) #将目标变量(2代表ADHD症状)的β, t, p存储
}
result_area_symptom<- as.data.frame(result_area_symptom)
colnames(result_area_symptom)[4] = 'pvalue'
result_area_symptom$P.Adj <- p.adjust(result_area_symptom$pvalue, method = 'fdr', n = length(result_area_symptom$pvalue))

result_area_inatten <- c()
for (i in 287:354){
  fit <- lmer(inattention ~ ABCD_T1_ECM_uncorrtced[,i] +
                sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 +
                as.factor(race_ethnicity) + smri_vol_scs_intracranialv +
                (1|site_id_l/rel_family_id),data = ABCD_T1_ECM_uncorrtced)
  result_area_inatten <- rbind(result_area_inatten,
                               c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,4,5)])) #将目标变量(2代表ADHD症状)的β, t, p存储
}
result_area_inatten <- as.data.frame(result_area_inatten)
colnames(result_area_inatten)[4] = 'pvalue'
result_area_inatten$P.Adj <- p.adjust(result_area_inatten$pvalue, method = 'fdr', n = length(result_area_inatten$pvalue))


result_area_implu <- c()
for (i in 287:354){
  fit <- lmer(impul_hyper~ ABCD_T1_ECM_uncorrtced[,i] +
                sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
                as.factor(race_ethnicity) +  smri_vol_scs_intracranialv +
                (1|site_id_l/rel_family_id),data = ABCD_T1_ECM_uncorrtced)
  result_area_implu <- rbind(result_area_implu,
                             c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,4,5)])) #将目标变量(2代表ADHD症状)的β, t, p存储
}
result_area_implu <- as.data.frame(result_area_implu)
colnames(result_area_implu)[4] = 'pvalue'
result_area_implu$P.Adj <- p.adjust(result_area_implu$pvalue, method = 'fdr', n = length(result_area_implu$pvalue))

####效应量的计算参考 "Association analysis between pathway measures and ADHD symptoms"