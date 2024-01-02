setwd("E:/Fudan_Luoqiang_ER to ADHD_Project/20220923_ABCD数据库信息/20221111_DataAnalysis/Outcome")
library(readxl)
library(lmerTest)
library(tidyverse)
library(effects)
library(effectsize)
library(simr)
library(lme4)
library(emmeans)
library(blme)
######如果涉及到多组被试或者交叉项，需要 emmeans包进行事后检验与简单效应分析
library(emmeans)
library(dplyr)

ABCD_多个维度的变量特征 <- read_excel("ABCD_多个维度的变量特征.xlsx")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- ABCD_多个维度的变量特征
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- as.data.frame(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2)

del1 <- which(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$Label == "NA")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[-del1,]

####### PSM -----
X20231214_ABCD_ER_ADHD_review <- read_excel("20231214_ABCD_ER_ADHD_review.xlsx", 
                                            +     sheet = "PropensityMatch")
PSM <- X20231214_ABCD_ER_ADHD_review
library(MatchIt)
m.out1 = matchit(Label ~ demo_prnt_ed_v2 + demo_comb_income_v2 + Age2 + sex2 + race_ethnicity
                 + site_id_l + TotalCognition + BAS, data = PSM, method = "nearest", ratio=1)
summary(m.out1)
plot(m.out1, type = "jitter")
plot(m.out1, type = "hist")

m.data1 <- match.data(m.out1)
write.csv(m.data1,"E:/Fudan_Luoqiang_ER to ADHD_Project/20220923_ABCD数据库信息/20221111_DataAnalysis/Outcome/PSM_New1.csv")



m.out2 = matchit(Label ~ demo_prnt_ed_v2 + demo_comb_income_v2 + Age2 + sex2 + race_ethnicity
                 + site_id_l + TotalCognition + BAS + BMI + A_puberty, data = PSM, method = "nearest", ratio=1)
summary(m.out2)
plot(m.out2, type = "jitter")
plot(m.out2, type = "hist")

m.data2 <- match.data(m.out2)
write.csv(m.data2,"E:/Fudan_Luoqiang_ER to ADHD_Project/20220923_ABCD数据库信息/20221111_DataAnalysis/Outcome/PSM_New2.csv")