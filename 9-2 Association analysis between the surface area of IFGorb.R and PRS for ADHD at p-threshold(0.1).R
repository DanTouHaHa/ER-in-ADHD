library(dplyr)
library(optimx)
library(performance)
library(readxl)
library(tidyverse)
setwd("E:/Fudan_Luoqiang_ADHD_Project/20220923_ABCD数据库信息/20230221_DataAnalysis/PRS")

PRS_0.001 <- read.table("ABCD.0.001.profile", header = T)
PRS_0.05 <- read.table("ABCD.0.05.profile", header = T)
PRS_0.1 <- read.table("ABCD.0.1.profile", header = T)
PRS_0.2 <- read.table("ABCD.0.2.profile", header = T)
PRS_0.3 <- read.table("ABCD.0.3.profile", header = T)
PRS_0.4 <- read.table("ABCD.0.4.profile", header = T)
PRS_0.5 <- read.table("ABCD.0.5.profile", header = T)

colnames(PRS_0.001) <- c("FID","src_subject_id","PHENO","CNT","CNT2","SCORE")
colnames(PRS_0.05) <- c("FID","src_subject_id","PHENO","CNT","CNT2","SCORE")
colnames(PRS_0.1) <- c("FID","src_subject_id","PHENO","CNT","CNT2","SCORE")
colnames(PRS_0.2) <- c("FID","src_subject_id","PHENO","CNT","CNT2","SCORE")
colnames(PRS_0.3) <- c("FID","src_subject_id","PHENO","CNT","CNT2","SCORE")
colnames(PRS_0.4) <- c("FID","src_subject_id","PHENO","CNT","CNT2","SCORE")
colnames(PRS_0.5) <- c("FID","src_subject_id","PHENO","CNT","CNT2","SCORE")
PRS_mean <- select(PRS_0.001,"FID","src_subject_id","PHENO","CNT","CNT2")
PRS_mean$meanPRSscore <- (PRS_0.001$SCORE + PRS_0.05$SCORE + PRS_0.1$SCORE + PRS_0.2$SCORE + PRS_0.3$SCORE + PRS_0.4$SCORE + PRS_0.5$SCORE)/7

pcs <- read.table("ABCD.eigenvec", header=F)
colnames(pcs) <- c("FID", "src_subject_id", paste0("PC",1:12))
ABCD_CBCL_Emotion_Cognition_Motivation_FL2 <- read_excel("ABCD_T1_ECM_uncorrtced_FDR.xlsx")

ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.001 <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2, PRS_0.001,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05 <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2, PRS_0.05,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1 <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2, PRS_0.1,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.2 <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2, PRS_0.2,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.3 <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2, PRS_0.3,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.4 <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2, PRS_0.4,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.5 <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2, PRS_0.5,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_mean <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2, PRS_mean,  by="src_subject_id")

ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.001_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.001, pcs,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05, pcs,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1, pcs,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.2_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.2, pcs,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.3_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.3, pcs,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.4_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.4, pcs,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.5_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.5, pcs,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_mean_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_mean, pcs,  by="src_subject_id")

library(readxl)
library(lmerTest)
library(tidyverse)
library(effects)
library(effectsize)
library(simr)
library(lme4)
library(emmeans)

####section mean_PC----
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[5:86] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[5:86], as.numeric)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$TotalCognition <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(6,7,8,11,12,13,25)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$Memory <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(12,25)], na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$EF <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(8,11)])
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$Verbal <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(7,13)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$Spatial <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(6)],na.rm = T)

ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$BISBAS <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(26:45)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$BIS <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(26:32)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$BAS <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(33:45)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$BAS_reward <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(33:37)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$BAS_drive <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(38:41)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC$BAS_funseeking <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[,c(42:45)],na.rm = T)


str(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC)

ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[50] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[50], as.numeric)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[51] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[51], as.numeric)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[59:68] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[59:68], as.numeric)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[73:74] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[73:74], as.numeric)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[75:78] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[75:78], as.numeric)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[75:78] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[75:78], as.numeric)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[90:113] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[90:113], as.numeric)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC <- select(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC,-c(FID.x, FID.y))
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC <- scale(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC)


library(blme)
library(optimx)

#section 脑区 PRS纳入模型----
#与Emotion相关的脑区
model_smri_area_cdk_parsobisrh <- glmer(smri_area_cdk_parsobisrh ~ SCORE +  #疑问 使用FL2的Age还是FL3的Age
                                         sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + 
                                        as.factor(race_ethnicity) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 +  scale(smri_vol_scs_intracranialv)
                                         (1|site_id_l/rel_family_id),data = ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC, family = "poisson")
summary(model_smri_area_cdk_parsobisrh)
summary(model_smri_area_cdk_parsobisrh)$coef
analysis_model_totalsymptom1 <- eta_squared(model_smri_area_cdk_parsobisrh)[,2]

ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC <- as.data.frame(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC)
####敏感性分析要控制的协变量
BMI <- read_excel("BMI匹配完被试.xlsx")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC, BMI,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[129] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[129], as.numeric)
Puberty <- read_excel("Puberty.xlsx")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC, Puberty,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[130:132] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC[130:132], as.numeric)

model_smri_area_cdk_parsobisrh_Sen <- glmer(smri_area_cdk_parsobisrh ~ SCORE +  #疑问 使用FL2的Age还是FL3的Age
                                          sex2 + scale(Age2) + demo_comb_income_v2 + scale(demo_prnt_ed_v2) + BMI + A_puberty +
                                          as.factor(race_ethnicity) + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 +  scale(smri_vol_scs_intracranialv)
                                          (1|site_id_l/rel_family_id),data = ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.1_PC, family = "poisson")
summary(model_smri_area_cdk_parsobisrh_Sen)
