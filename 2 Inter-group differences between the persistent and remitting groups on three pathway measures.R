
#section 数据清洗----
setwd("E:/Fudan_Luoqiang_ADHD_Project/20220923_ABCD数据库信息/20221111_DataAnalysis/机器学习分类/Persistent or remitted/机器学习")
library(readxl)
library(lmerTest)
library(tidyverse)
library(effects)
library(effectsize)
library(simr)
library(lme4)
######如果涉及到多组被试或者交叉项，需要 emmeans包进行事后检验与简单效应分析
library(emmeans)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- read_excel("ABCD_T1_ECM_uncorrtced_FDR_Med_65_last.xlsx")

###### LMM模型, 6046个被试
str(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)
del1 <- which(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$rawscore == "NA")
del2 <- which(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$tscore == "NA")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- as.data.frame(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)

#####敏感性分析需要额外控制的协变量
BMI <- read_excel("BMI匹配完被试.xlsx")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- left_join(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2, BMI,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[20] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[20], as.numeric)
Puberty <- read_excel("Puberty.xlsx")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- left_join(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2, Puberty,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[21:23] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[21:23], as.numeric)

#####三通路测量的组间差异
result_pathway <- c()
for (i in 99:101){
  fit <- lmer(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[,i] ~ persistentORnot + 
                sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 +
                as.factor(race_ethnicity) + 
                (1|site_id_l/rel_family_id),data = ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)
  result_pathway  <- rbind(result_pathway ,
                           c(colnames(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)[i], coef(summary(fit))[2,c(1,4,5)])) #将目标变量(2代表emotion)的β, t, p存储
}
result_pathway <- as.data.frame(result_pathway)
colnames(result_pathway )[4] = 'pvalue'
result_pathway$P.Adj <- p.adjust(result_pathway$pvalue, method = 'fdr', n = length(result_pathway$pvalue))

########服药人群中三通路测量的组间差异
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1 <- read_excel("ABCD_CBCL_Emotion_Cognition_Motivation_FL2_Med_65_last.xlsx")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1 <- as.data.frame(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1)
#####敏感性分析需要额外控制的协变量
BMI <- read_excel("BMI匹配完被试.xlsx")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1 <- left_join(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1, BMI,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1[102] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1[102], as.numeric)
Puberty <- read_excel("Puberty.xlsx")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1 <- left_join(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1, Puberty,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1[103:105] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1[103:105], as.numeric)

result_pathway_1 <- c()
for (i in 99:101){
  fit <- lmer(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1[,i] ~ persistentORnot + 
                sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
                as.factor(race_ethnicity) + 
                (1|site_id_l/rel_family_id),data = ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1)
  result_pathway_1  <- rbind(result_pathway_1 ,
                             c(colnames(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2_1)[i], coef(summary(fit))[2,c(1,4,5)])) #将目标变量(2代表emotion)的β, t, p存储
}
result_pathway_1 <- as.data.frame(result_pathway_1)
colnames(result_pathway_1 )[4] = 'pvalue'
result_pathway_1$P.Adj <- p.adjust(result_pathway_1$pvalue, method = 'fdr', n = length(result_pathway_1$pvalue))
