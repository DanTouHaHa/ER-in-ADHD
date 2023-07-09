##### 对于ADHD 症状 (2次)，emotion, cognition, motivation，采用Bayes网络确定方向。
##### 协变量包括 age, sex, income, race, site, prnt_ed, family   (Motivation采用BISBAS或者BAS)
##### https://github.com/Adi5598/Bayesian-Networks github----
#section 数据清洗----
setwd("E:/Fudan_Luoqiang_ADHD_Project/20220923_ABCD数据库信息/20221031_DataAnalysis/行为数据2")
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
#section 数据清洗----
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
##删除y为空值的行
del1 <- which(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$rawscore == "NA") ###与emotion缺失的ID不同，所以全都删了
del2 <- which(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$tscore == "NA")
del3 <- which(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$TotalEmotionScore == "NA") ##缺失值最多，按照这个删除
del4 <- which(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$BISBAS == "NA")
del5 <- which(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$rawscore_FL2 == "NA")
del6 <- union(del1, del3)  #取并集
del7 <- union(del6,del5)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[-del7,]
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[50] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[50], as.numeric)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[51] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[51], as.numeric)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[59:68] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[59:68], as.numeric)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[73:74] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[73:74], as.numeric)

#每个家庭抽取一个人
A <- unique(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$rel_family_id)
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[!duplicated(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2$rel_family_id),]

#敏感性分析需要额外控制的协变量
BMI <- read_excel("BMI匹配完被试.xlsx")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- left_join(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2, BMI,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[88] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[88], as.numeric)
Puberty <- read_excel("Puberty.xlsx")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2 <- left_join(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2, Puberty,  by="src_subject_id")
ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[89:91] <- lapply(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2[89:91], as.numeric)



#section 先控制协变量，得到残差进行Bayes----
###### 需要先控制掉协变量，将需要进行推断的变量先回归协变量,      敏感性分析额外控制BMI和puberty
library(bnlearn)
library(gtools)
###选取需要的多列
Data <- select(ABCD_CBCL_Emotion_Cognition_MOtivation_FL2, BAS,TotalCognition, TotalEmotionScore,
               rawscore, rawscore_FL2, Age2, sex2, demo_comb_income_v2, inattention, impul_hyper, BMI, A_puberty,
               demo_prnt_ed_v2, race_ethnicity,  site_id_l, rel_family_id)

model_BAS <- lm(BAS ~ Age2 + sex2 + demo_comb_income_v2 +   
                  demo_prnt_ed_v2 + as.factor(race_ethnicity) + as.factor(site_id_l) , 
                data = Data)
summary(model_BAS)
res_BAS = model_BAS$residuals

model_Emotion <- lm(TotalEmotionScore ~ Age2 + sex2 + demo_comb_income_v2 +
                      demo_prnt_ed_v2 + as.factor(race_ethnicity) + as.factor(site_id_l) , 
                    data = Data)
summary(model_Emotion)
res_Emotion = model_Emotion$residuals

model_Cognition <- lm(scale(TotalCognition) ~ Age2 + sex2 + demo_comb_income_v2 +
                        demo_prnt_ed_v2 + as.factor(race_ethnicity) + as.factor(site_id_l), 
                      data = Data)
summary(model_Cognition)
res_Cognition = model_Cognition$residuals

model_Rawscore <- lm(rawscore ~ Age2 + sex2 + demo_comb_income_v2 + 
                       demo_prnt_ed_v2 + as.factor(race_ethnicity) + as.factor(site_id_l) , 
                     data = Data)
summary(model_Rawscore)
res_Rawscore = model_Rawscore$residuals


model_inattention <- lm(inattention ~ Age2 + sex2 + demo_comb_income_v2 + 
                          demo_prnt_ed_v2 + as.factor(race_ethnicity) +as.factor(site_id_l) , 
                        data = Data)
summary(model_inattention)
res_inattention = model_inattention$residuals

model_impul <- lm(impul_hyper ~ Age2 + sex2 + demo_comb_income_v2 + 
                          demo_prnt_ed_v2 + as.factor(race_ethnicity) + as.factor(site_id_l)  , 
                        data = Data)
summary(model_impul)
res_impul = model_impul$residuals

#####将4个values合并成dataframe，    对于Inattention和hyperactivity-impulsivity只需要替换掉rea_Rawscore这个变量
Data_Bayes_BAS <- cbind(res_BAS,res_Cognition,res_Emotion,res_Rawscore)
Data_Bayes_BAS <- as.data.frame(Data_Bayes_BAS)
variables_BAS <- c('res_BAS','res_Cognition','res_Emotion','res_Rawscore')


# Chosing a subset of variables: 'res_BAS','res_BISBAS','res_Cognition','res_Emotion','res_Rawscore'
final_variables_BAS <- c(1,2,3,4)
Data_Bayes_BAS = Data_Bayes_BAS[,final_variables_BAS]

#Splitting the data into training and testing.
#library(scorecard)
#set.seed(100)
#Data_Bayes_BAS <- split_df(Data_Bayes_BAS,ratio = 0.8)
#training <- Data_Bayes_BAS$train
#testing  <- Data_Bayes_BAS$test

Data_Bayes_BAS <- as.data.frame(Data_Bayes_BAS)
######################### BUILDING THE MODEL ###############################

# Reference : https://www.youtube.com/watch?v=6pl3m-UbUV4
alogorithms <- c('hc')
scores <- c('bge')
for(algo in alogorithms){
  for(score in scores){
    print(algo)
    print(score)
    models <- boot.strength(data = Data_Bayes_BAS,R=5000,algorithm = algo,algorithm.args = list(score=score))
    best_models <- averaged.network(models,threshold = 0.7)
    fitted_model <- bn.fit(best_models,data = Data_Bayes_BAS)
    Learned_Bayesian_network <- graphviz.plot(fitted_model,highlight = list('fill'),layout = 'fdp',render = TRUE)
  }
}