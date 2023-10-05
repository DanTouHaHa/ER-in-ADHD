####以PRS-th 0.05为例子计算PRS打分与IFGorb.R脑区表面积的关系

library(dplyr)
library(optimx)
library(performance)
library(readxl)
library(tidyverse)
setwd("E:/Fudan_Luoqiang_ADHD_Project/20220923_ABCD数据库信息/20230419_DataAnalysis/PRS")

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
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[3:59] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[3:59], as.numeric)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$TotalCognition <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(5:14)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$Memory <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(7,10,13)], na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$EF <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(6,8,9)])
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$Verbal <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(5,11)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$Spatial <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(12,14)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$NIH <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(5:11)],na.rm = T)

ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$BISBAS <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(21:40)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$BIS <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(21:27)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$BAS <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(28:40)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$BAS_reward <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(28:32)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$BAS_drive <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(33:36)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$BAS_funseeking <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(37:40)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$Inattention_3 <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(52:54)],na.rm = T)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$Hyper_3 <- rowSums(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[,c(55:58)],na.rm = T)

str(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC)
del1 <- which(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$rawscore_FL3 == "NA")
del2 <- which(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$tscore_FL3 == "NA")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC <- select(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC,-c("FID.x", "FID.y"))
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC <- scale(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC <- as.data.frame(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC)

#####敏感性分析需要额外控制的协变量
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC <- as.data.frame(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC)
BMI <- read_excel("BMI匹配完被试.xlsx")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC, BMI,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[132] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[132], as.numeric)
Puberty <- read_excel("Puberty.xlsx")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC <- left_join(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC, Puberty,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[133:135] <- lapply(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[133:135], as.numeric)


library(blme)
library(optimx)

#抽人
A <- unique(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$rel_family_id)
ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC <- ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC[!duplicated(ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC$rel_family_id),]

#section 脑区 PRS纳入模型----
#以阈值p = 0.05为例
model_smri_area_cdk_parsobisrh <- lmer(smri_area_cdk_parsobisrh ~ SCORE +  #疑问 使用FL2的Age还是FL3的Age
                                              sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + scale(smri_vol_scs_intracranialv)+ 
                                              (1|site_id_l),data = ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC)
summary(model_smri_area_cdk_parsobisrh)
summary(model_smri_area_cdk_parsobisrh)$coef
analysis_model_totalsymptom1 <- eta_squared(model_smri_area_cdk_parsobisrh)[,2]

####敏感性分析
model_smri_area_cdk_parsobisrh <- lmer(smri_area_cdk_parsobisrh ~ SCORE +  #疑问 使用FL2的Age还是FL3的Age
                                              sex2 + Age2 + demo_comb_income_v2 + demo_prnt_ed_v2 + scale(smri_vol_scs_intracranialv) +  BMI + A_puberty+
                                              (1|site_id_l),data = ABCD_CBCL_Emotion_Cognition_Motivation_FL2_PRS_0.05_PC)
summary(model_smri_area_cdk_parsobisrh)
summary(model_smri_area_cdk_parsobisrh)$coef
analysis_model_totalsymptom1 <- eta_squared(model_smri_area_cdk_parsobisrh)[,2]
