setwd("E:/Fudan_Luoqiang_ADHD_Project/20220923_ABCD数据库信息/20221111_DataAnalysis/影像数据")
library(readxl)
ABCD_T1_ECM_uncorrtced <- read_excel("ABCD_T1_ECM_uncorrtced_FDR.xlsx")

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
ABCD_T1_ECM_uncorrtced[86] <- lapply(ABCD_T1_ECM_uncorrtced[86], as.numeric)
ABCD_T1_ECM_uncorrtced[84:86] <- lapply(ABCD_T1_ECM_uncorrtced[84:86], as.numeric)


#section ADHD症状----
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)
#敏感性分析需要额外控制的协变量
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)
BMI <- read_excel("BMI匹配完被试.xlsx")
ABCD_T1_ECM_uncorrtced <- left_join(ABCD_T1_ECM_uncorrtced, BMI,  by="src_subject_id")
ABCD_T1_ECM_uncorrtced[106] <- lapply(ABCD_T1_ECM_uncorrtced[106], as.numeric)
Puberty <- read_excel("Puberty.xlsx")
ABCD_T1_ECM_uncorrtced <- left_join(ABCD_T1_ECM_uncorrtced, Puberty,  by="src_subject_id")
ABCD_T1_ECM_uncorrtced[107:109] <- lapply(ABCD_T1_ECM_uncorrtced[107:109], as.numeric)

#section ECM相互控制之后对应的神经相关性----
### 287--354, area--emotion
result_area_ECM_E <- c()
for (i in 73:78){
  fit <- lmer(ABCD_T1_ECM_uncorrtced[,i] ~ TotalEmotionScore + TotalCognition + BAS +
                sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
                as.factor(race_ethnicity)  + smri_vol_scs_intracranialv + 
                (1|site_id_l/rel_family_id),data = ABCD_T1_ECM_uncorrtced)
  result_area_ECM_E <- rbind(result_area_ECM_E,
                               c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[2,c(1,4,5)])) #将目标变量(2代表emotion)的β, t, p存储
}
result_area_ECM_E <- as.data.frame(result_area_ECM_E)
colnames(result_area_ECM_E)[4] = 'pvalue'
result_area_ECM_E$P.Adj <- p.adjust(result_area_ECM_E$pvalue, method = 'fdr', n = length(result_area_ECM_E$pvalue))

### 287--354, area_cognition
result_area_ECM_C <- c()
for (i in 73:78){
  fit <- lmer(ABCD_T1_ECM_uncorrtced[,i] ~ TotalEmotionScore + TotalCognition + BAS +
                sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
                as.factor(race_ethnicity)  + smri_vol_scs_intracranialv + 
                (1|site_id_l/rel_family_id),data = ABCD_T1_ECM_uncorrtced)
  result_area_ECM_C <- rbind(result_area_ECM_C,
                                 c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[3,c(1,4,5)])) #将目标变量(3代表cognition)的β, t, p存储
}
result_area_ECM_C <- as.data.frame(result_area_ECM_C)
colnames(result_area_ECM_C)[4] = 'pvalue'
result_area_ECM_C$P.Adj <- p.adjust(result_area_ECM_C$pvalue, method = 'fdr', n = length(result_area_ECM_C$pvalue))

### 287--254, area_motivation
result_area_ECM_M <- c()
for (i in 73:78){
  fit <- lmer(ABCD_T1_ECM_uncorrtced[,i] ~ TotalEmotionScore + TotalCognition + BAS +
                sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 +
                as.factor(race_ethnicity) + smri_vol_scs_intracranialv +  
                (1|site_id_l/rel_family_id),data = ABCD_T1_ECM_uncorrtced)
  result_area_ECM_M <- rbind(result_area_ECM_M,
                                  c(colnames(ABCD_T1_ECM_uncorrtced)[i], coef(summary(fit))[4,c(1,4,5)])) #将目标变量(4代表motivation)的β, t, p存储
}
result_area_ECM_M <- as.data.frame(result_area_ECM_M)
colnames(result_area_ECM_M)[4] = 'pvalue'
result_area_ECM_M$P.Adj <- p.adjust(result_area_ECM_M$pvalue, method = 'fdr', n = length(result_area_ECM_M$pvalue))

#####效应量的计算举例
library('EMAtools')
lme.dscore(	smri_area_cdk_parsobisrh ~ TotalEmotionScore + 
             TotalCognition + BAS + 
             sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
             as.factor(race_ethnicity) + smri_vol_scs_intracranialv +
             (1|site_id_l/rel_family_id),data=ABCD_T1_ECM_uncorrtced,type="lme4") #,type="lme4","nlme"
# etasquared
effectsize::eta_squared(model_impul)
