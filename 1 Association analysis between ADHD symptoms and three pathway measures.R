
#section 数据清洗----
setwd("E:/Fudan_Luoqiang_ADHD_Project/20220923_ABCD数据库信息/20221111_DataAnalysis/行为数据/Sensivity analysis")
library(readxl)
library(lmerTest)
library(tidyverse)
library(effects)
library(effectsize)
library(simr)
library(lme4)
######如果涉及到多组被试或者交叉项，需要 emmeans包进行事后检验与简单效应分析
library(emmeans)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- read_excel("ABCD_CBCL_Emotion_Cognition_Motivation_FL2.xlsx")
###### 认知、情绪、动机、症状区分维度，先都按照不区分维度进行计算。
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$TotalCognition <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(6,7,8,11,12,13,25)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$Memory <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(12,25)], na.rm = T)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$EF <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(8,11)])
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$Verbal <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(7,13)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$Spatial <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(6)],na.rm = T)

ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$BISBAS <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(26:45)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$BIS <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(26:32)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$BAS <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(33:45)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$BAS_reward <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(33:37)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$BAS_drive <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(38:41)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$BAS_funseeking <- rowSums(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,c(42:45)],na.rm = T)

###### LMM模型, 6046个被试
str(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)
del1 <- which(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$rawscore == "NA")
del2 <- which(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$tscore == "NA")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[-del1,]

ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[50] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[50], as.numeric)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[51] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[51], as.numeric)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[59:68] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[59:68], as.numeric)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[73:74] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[73:74], as.numeric)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[75:78] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[75:78], as.numeric)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[79:86] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[79:86], as.numeric)

#敏感性分析需要额外控制的协变量
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- read_excel("ABCD_CBCL_Emotion_Cognition_Motivation_FL2.xlsx")
BMI <- read_excel("BMI匹配完被试.xlsx")
Puberty <- read_excel("Puberty.xlsx")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- left_join(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2, BMI,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[98] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[98], as.numeric)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- left_join(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2, Puberty,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[99:101] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[99:101], as.numeric)

#section ECM同时纳入模型----
##### 1.1 ECM同时纳入模型，相互控制
##### 1.1.1 total symptom,  abcd_cbcls01: cbcl_scr_dsm5_adhd_r，       敏感性分析额外控制BMI和puberty
model_totalsymptom <- lmer(rawscore ~ TotalEmotionScore + 
                             TotalCognition + BAS + 
                             sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
                           race_ethnicity+
                             (1|site_id_l/rel_family_id),data = ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)
summary(model_totalsymptom)
summary(model_totalsymptom)$coef
analysis_model_totalsymptom <- eta_squared(model_totalsymptom)[,2]

#section 各种效应量 ----
performance::r2(model_totalsymptom)
#Cohen's d
library('EMAtools')
lme.dscore(rawscore ~ TotalEmotionScore + 
             TotalCognition + BAS + 
             sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
             as.factor(race_ethnicity)+ 
             (1|site_id_l/rel_family_id),data=ABCD_CBCL_Emotion_Cognition_MOtivation_FL2,type="lme4")
# etasquared
effectsize::eta_squared(model_totalsymptom)
#Standard β

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}
fixef(model_totalsymptom)

#0.95CI
confint(model_totalsymptom, level = 0.95)

##### 1.1.2 Inattention, abcd_clcl01: inattention(cbcl_q04;cbcl_q08;cbcl_q78)
model_inattention <- lmer(inattention ~ TotalEmotionScore + 
                            TotalCognition + BAS +
                            sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 +
                            as.factor(race_ethnicity) + 
                            (1|site_id_l/rel_family_id),data = ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)
summary(model_inattention)
summary(model_inattention)$coef
analysis_model_inattention1 <- eta_squared(model_inattention)[,2]

## R方
performance::r2(model_inattention)
#Cohen's d
library('EMAtools')
lme.dscore(inattention ~ TotalEmotionScore +
             TotalCognition + BAS + 
             sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
             as.factor(race_ethnicity) +
             (1|site_id_l/rel_family_id),data=ABCD_CBCL_Emotion_Cognition_MOtivation_FL2,type="lme4")
# etasquared
effectsize::eta_squared(model_inattention)
#Standard β

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}
fixef(model_inattention)
confint(model_inattention, level = 0.95)

##### 1.1.3 Hyperactivity/impulsivity, abcd_clcl01: Hyper/impul(cbcl_q10;cbcl_q41;cbcl_q93;cbcl_q104)
model_impul <- lmer(impul_hyper ~ TotalEmotionScore + 
                      TotalCognition + BAS +
                      sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
                      as.factor(race_ethnicity) + 
                      (1|site_id_l/rel_family_id),data = ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)
summary(model_impul)
summary(model_impul)$coef
analysis_model_impul1 <- eta_squared(model_impul)[,2]

performance::r2(model_impul)

fixef(model_impul)

library('EMAtools')
lme.dscore(impul_hyper ~ TotalEmotionScore + 
             TotalCognition + BAS + 
             sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
             as.factor(race_ethnicity) + 
             (1|site_id_l/rel_family_id),data=ABCD_CBCL_Emotion_Cognition_MOtivation_FL2,type="lme4")
# etasquared
effectsize::eta_squared(model_impul)

confint(model_impul, level = 0.95)

####1.1.4 emotion cognition 和motivation的关系
model_1<- lmer(TotalEmotionScore ~ 
                            TotalCognition + BAS +
                            sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
                            as.factor(race_ethnicity) + 
                            (1|site_id_l/rel_family_id),data = ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)
summary(model_1)

####1.15 因变量为ADHD_FU2的LMM模型
model_totalsymptom <- lmer(rawscore_FL2 ~ TotalEmotionScore + 
                             TotalCognition + BAS + 
                             sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 +  
                            as.factor(race_ethnicity) +
                             (1|site_id_l/rel_family_id),data = ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)
summary(model_totalsymptom)
effectsize::eta_squared(model_totalsymptom)
performance::r2(model_totalsymptom)
confint(model_totalsymptom, level = 0.95)
