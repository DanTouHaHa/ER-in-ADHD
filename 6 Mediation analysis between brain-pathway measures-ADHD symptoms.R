
#section 数据清洗----
setwd("E:/Fudan_Luoqiang_ADHD_Project/20220923_ABCD数据库信息/20221111_DataAnalysis/影像数据")
library(readxl)
library(lmerTest)
library(tidyverse)
library(effects)
library(effectsize)
library(simr)
library(lme4)
######如果涉及到多组被试或者交叉项，需要 emmeans包进行事后检验与简单效应分析
library(emmeans)
ABCD_T1_ECM_uncorrtced <- read_excel("ABCD_T1_ECM_Mediation_uncorrtced.xlsx")
###### 认知按照不调整的数据计算
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

###### LMM模型, 6046个被试
str(ABCD_T1_ECM_uncorrtced)
del1 <- which(ABCD_T1_ECM_uncorrtced$rawscore == "NA")
del2 <- which(ABCD_T1_ECM_uncorrtced$tscore == "NA")
del3 <- which(ABCD_T1_ECM_uncorrtced$inattention == "NA")
del4 <- which(ABCD_T1_ECM_uncorrtced$impul_hyper == "NA")
# ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[-del1,]
ABCD_T1_ECM_uncorrtced[50] <- lapply(ABCD_T1_ECM_uncorrtced[50], as.numeric)
ABCD_T1_ECM_uncorrtced[51] <- lapply(ABCD_T1_ECM_uncorrtced[51], as.numeric)
ABCD_T1_ECM_uncorrtced[59:68] <- lapply(ABCD_T1_ECM_uncorrtced[59:68], as.numeric)
ABCD_T1_ECM_uncorrtced[73:74] <- lapply(ABCD_T1_ECM_uncorrtced[73:74], as.numeric)


###敏感性分析需要额外控制的协变量
ABCD_T1_ECM_uncorrtced <- as.data.frame(ABCD_T1_ECM_uncorrtced)
BMI <- read_excel("BMI匹配完被试.xlsx")
ABCD_T1_ECM_uncorrtced <- left_join(ABCD_T1_ECM_uncorrtced, BMI,  by="src_subject_id")
ABCD_T1_ECM_uncorrtced[106] <- lapply(ABCD_T1_ECM_uncorrtced[106], as.numeric)
Puberty <- read_excel("Puberty.xlsx")
ABCD_T1_ECM_uncorrtced <- left_join(ABCD_T1_ECM_uncorrtced, Puberty,  by="src_subject_id")
ABCD_T1_ECM_uncorrtced[107:109] <- lapply(ABCD_T1_ECM_uncorrtced[107:109], as.numeric)

#section 中介模型 ----
##### 协变量(Age2,sex2,demo_comb_income_v2,demo_prnt_ed_v2,race_ethnicity,site_id_l
##### 主要变量三个通道(Emotion,Cognition,Motvation(BAS),症状,Brain)
##### https://zhuanlan.zhihu.com/p/376007591
library(mediation)
library(mediationsens)
library(stargazer)
library(bruceR) #用于很多个中介模型，循环

#每个家庭一个人
A <- unique(ABCD_T1_ECM_uncorrtced$rel_family_id)
ABCD_T1_ECM_uncorrtced <- ABCD_T1_ECM_uncorrtced[!duplicated(ABCD_T1_ECM_uncorrtced$rel_family_id),]

#中介模型的数据标准化
ABCD_T1_ECM_uncorrtced$scale_rawscore <- scale(ABCD_T1_ECM_uncorrtced$rawscore)
ABCD_T1_ECM_uncorrtced$scale_TotalEmotionScore <- scale(ABCD_T1_ECM_uncorrtced$TotalEmotionScore)
ABCD_T1_ECM_uncorrtced$scale_par <- scale(ABCD_T1_ECM_uncorrtced$smri_area_cdk_parsobisrh)
ABCD_T1_ECM_uncorrtced$scale_BAS <- scale(ABCD_T1_ECM_uncorrtced$BAS)
ABCD_T1_ECM_uncorrtced$scale_Cog <- scale(ABCD_T1_ECM_uncorrtced$TotalCognition)
ABCD_T1_ECM_uncorrtced$scale_ift <- scale(ABCD_T1_ECM_uncorrtced$smri_area_cdk_iftmlh)
ABCD_T1_ECM_uncorrtced$scale_rrmd <- scale(ABCD_T1_ECM_uncorrtced$smri_area_cdk_rrmdfrrh)
ABCD_T1_ECM_uncorrtced$scale_inattention <- scale(ABCD_T1_ECM_uncorrtced$inattention)
ABCD_T1_ECM_uncorrtced$scale_impul <- scale(ABCD_T1_ECM_uncorrtced$impul_hyper)
ABCD_T1_ECM_uncorrtced[110:118] <- lapply(ABCD_T1_ECM_uncorrtced[110:118], as.numeric)

#####中介模型举例，race和site变量为类别变量
model_1 <- PROCESS(ABCD_T1_ECM_uncorrtced, y = "scale_inattention", meds = "scale_TotalEmotionScore", 
        x ="scale_par", #中介变量，可以是多个，用c()
        covs = c("Age2", "sex2","demo_comb_income_v2","demo_prnt_ed_v2","smri_vol_scs_intracranialv", 
                 "scale_BAS", "scale_Cog",
                 "race_ethnicity", "site_id_l"), #协变量
        ci = "boot", nsim = 5000, seed = 1)
model_1[["results"]][[1]][["mediation"]]

