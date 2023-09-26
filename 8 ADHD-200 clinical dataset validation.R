setwd("E:/Fudan_Luoqiang_ADHD_Project/20221203_ADHD200数据库/202302/数据分析/1Handness等问题")
library(readxl)
ADHD_200 <- read_excel("ADHD200数据分析hand_01_718.xlsx")
#方差分析: 协变量：(age, sex, handedness, total intracranial volume, IQ, and site)  
str(ADHD_200) 
ADHD_200[5:20] <- lapply(ADHD_200[5:20], as.numeric)
ADHD_200 <- as.data.frame(ADHD_200)
#其中NC:409人，ADHD:共263人。
library(lme4)
library(lmerTest)
#section ADHD(1) vs NC(0) ----
#surface area
result_ADHD_NC_SA <- c()
for (i in 26:31){
  fit <- lm(scale(ADHD_200[,i]) ~ ADHDorNot +
                Gender + Age + Handedness + as.factor(Site) + TCV + Full4IQ,data = ADHD_200)
  result_ADHD_NC_SA <- rbind(result_ADHD_NC_SA,
                                c(colnames(ADHD_200)[i], coef(summary(fit))[2,c(1,3,4)])) #将目标变量(2代表ADHD症状)的β, t, p存储
}
result_ADHD_NC_SA <- as.data.frame(result_ADHD_NC_SA)
colnames(result_ADHD_NC_SA)[4] = 'pvalue'
result_ADHD_NC_SA$P.Adj <- p.adjust(result_ADHD_NC_SA$pvalue, method = 'fdr', n = length(result_ADHD_NC_SA$pvalue))


#####机器学习-----LDA模型
# install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
setwd("E:/Fudan_Luoqiang_ADHD_Project/20220923_ABCD数据库信息/20221111_DataAnalysis/机器学习分类/Persistent or remitted/机器学习")

library(readxl)
ABCD_T1_ECM_uncorrtced_FDR_65_delete <- read_excel("ABCD_T1_ECM_uncorrtced_FDR_Med_65_last_class.xlsx")
library(caret)
inTrain = createDataPartition(ABCD_T1_ECM_uncorrtced_FDR_65_delete$persistentORnot, p=3/4,list = F)
newdata4 <- ABCD_T1_ECM_uncorrtced_FDR_65_delete
trainx = newdata4[inTrain,]
trainx <- trainx[,3:13]
testx = newdata4[-inTrain,]
testx <- testx[,3:13]
trainy = ABCD_T1_ECM_uncorrtced_FDR_65_delete$persistentORnot[inTrain]
testy = ABCD_T1_ECM_uncorrtced_FDR_65_delete$persistentORnot[-inTrain]
fitControl = trainControl(method = "repeatedcv", number = 5, repeats = 200,returnResamp = "all")
#gbmGrid = expand.grid(.interaction.depth = c(1, 3),.n.trees = c(50, 100, 150, 200, 250, 300),.shrinkage = 0.1)
library("gbm")
gbmFit1 = train(trainx,as.factor(trainy),method = "lda",trControl = fitControl) 
predict(gbmFit1, newdata = testx,interval="prediction")
models = list(gbmFit1)
predValues = extractPrediction(models,testX = testx, testY = testy)
head(predValues)
testValues = subset(predValues, dataType == "Test")
probValues = extractProb(models,testX = testx, testY = testy)
testProbs = subset(probValues, dataType == "Test")
Pred1 = subset(testValues, model == "lda")
confusionMatrix(Pred1$pred, Pred1$obs)
prob1 = subset(testProbs, model == "lda")
library(ROCR)
prob1$lable=ifelse(prob1$obs=='active',yes=1,0)
pred1 = prediction(prob1$active,prob1$lable)
perf1 = performance(pred1, measure="tpr", x.measure="fpr" )
plot(perf1, col ="#c5db90",lwd = 2.5 )  #  #86a2d1 #c5db90 #d4b179
#计算AUC
auc<-  performance(pred1,"auc")
auc_area<-slot(auc,"y.values")[[1]]; #auc_area<-auc@"y.values"[[1]];
auc_area<-round(auc_area,4)


#####机器学习-----GBM模型
# install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
setwd("E:/Fudan_Luoqiang_ADHD_Project/20220923_ABCD数据库信息/20221111_DataAnalysis/机器学习分类/Persistent or remitted/机器学习")

library(readxl)
ABCD_T1_ECM_uncorrtced_FDR_65_delete <- read_excel("ABCD_T1_ECM_uncorrtced_FDR_Med_65_last_class.xlsx")
library(caret)
inTrain = createDataPartition(ABCD_T1_ECM_uncorrtced_FDR_65_delete$persistentORnot, p=3/4,list = F)
newdata4 <- ABCD_T1_ECM_uncorrtced_FDR_65_delete
trainx = newdata4[inTrain,]
trainx <- trainx[,3:13]
testx = newdata4[-inTrain,]
testx <- testx[,3:13]
trainy = ABCD_T1_ECM_uncorrtced_FDR_65_delete$persistentORnot[inTrain]
testy = ABCD_T1_ECM_uncorrtced_FDR_65_delete$persistentORnot[-inTrain]
fitControl = trainControl(method = "repeatedcv", number = 5, repeats = 200,returnResamp = "all")
#gbmGrid = expand.grid(.interaction.depth = c(1, 3),.n.trees = c(50, 100, 150, 200, 250, 300),.shrinkage = 0.1)
library("gbm")
gbmFit1 = train(trainx,as.factor(trainy),method = "gbm",trControl = fitControl) 
predict(gbmFit1, newdata = testx,interval="prediction")
models = list(gbmFit1)
predValues = extractPrediction(models,testX = testx, testY = testy)
head(predValues)
testValues = subset(predValues, dataType == "Test")
probValues = extractProb(models,testX = testx, testY = testy)
testProbs = subset(probValues, dataType == "Test")
Pred1 = subset(testValues, model == "gbm")
confusionMatrix(Pred1$pred, Pred1$obs)
prob1 = subset(testProbs, model == "gbm")
library(ROCR)
prob1$lable=ifelse(prob1$obs=='active',yes=1,0)
pred1 = prediction(prob1$active,prob1$lable)
perf1 = performance(pred1, measure="tpr", x.measure="fpr" )
plot(perf1, col ="#c5db90",lwd = 2.5 )  #  #86a2d1 #c5db90 #d4b179
#计算AUC
auc<-  performance(pred1,"auc")
auc_area<-slot(auc,"y.values")[[1]]; #auc_area<-auc@"y.values"[[1]];
auc_area<-round(auc_area,4)
