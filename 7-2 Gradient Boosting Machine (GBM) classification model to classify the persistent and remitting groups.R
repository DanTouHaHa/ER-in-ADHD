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
