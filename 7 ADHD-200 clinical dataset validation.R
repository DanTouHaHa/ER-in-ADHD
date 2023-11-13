setwd("E:/Fudan_Luoqiang_ADHD_Project/20221203_ADHD200数据库/202302/数据分析/1Handness等问题")
library(readxl)
ADHD_200 <- read_excel("ADHD200数据分析hand_01_718.xlsx")
#方差分析: 协变量：(age, sex, handedness, total intracranial volume, IQ, and site)  
str(ADHD_200) 
ADHD_200[5:20] <- lapply(ADHD_200[5:20], as.numeric)
ADHD_200 <- as.data.frame(ADHD_200)
#共计592人；其中NC:419人，ADHD:共173人。
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

#### linear regression ##### Python
import pandas as pd
import numpy as np
from sklearn.model_selection import KFold
from sklearn.linear_model import LinearRegression
from scipy.stats import pearsonr
#随机抽样200次，每次5折，产生1000个r

def fold5(x, x_less, y):
    # 将数据拆成5份
    kf = KFold(n_splits=5, shuffle=True)
    x_train_list=[]
    xLess_train_list=[]
    y_train_list=[]
    x_test_list=[]
    xLess_test_list=[]
    y_test_list=[]
    for i, (train_index, test_index) in enumerate(kf.split(x)):
        print("第{}折".format(i))
        # print(train_index)
        # print(test_index)
        # print(x[test_index])
        x_train_list.append(x[train_index])
        y_train_list.append(y[train_index])
        xLess_train_list.append(x_less[train_index])
        x_test_list.append(x[test_index])
        xLess_test_list.append(x_less[test_index])
        y_test_list.append(y[test_index])

    return x_train_list, y_train_list, x_test_list, y_test_list, xLess_train_list, xLess_test_list

## linear regression 
data=pd.read_excel("ADHD_200 patients prediction 1.xlsx")

data_X=data[["Gender", "Age", "lh_SA_inferiortemporal", "rh_SA_parsorbitalis", "rh_SA_rostralmiddlefrontal"]]
data_X_less=data[["Gender", "Age", "lh_SA_inferiortemporal", "rh_SA_rostralmiddlefrontal"]]
data_Y=data["Inattentive"]  # 设置Y变量

result=pd.DataFrame()
for i in range(200):
    temp=dict()
    X_train_list, Y_train_list, X_test_list, Y_test_list, XLess_train_list, XLess_test_list=fold5(data_X.values, data_X_less.values, data_Y.values)
    for x_train, y_train, x_test, y_test, xLess_train, xLess_test in zip(X_train_list, Y_train_list, X_test_list, Y_test_list, XLess_train_list, XLess_test_list):
        reg = LinearRegression() # 线性回归
        reg.fit(x_train, y_train)

        regLess=LinearRegression() # 线性回归
        regLess.fit(xLess_train, y_train)

        print("训练系数:\n", reg.coef_) # 输出系数
        # print(reg.predict(x_test).shape)
        # print(y_test.shape)
        corr, pvalue=pearsonr(reg.predict(x_test), y_test)
        corrLess, pvalueLess=pearsonr(regLess.predict(xLess_test), y_test)

        temp["相关性系数"]=corr
        temp["显著性"]=pvalue
        print("相关性系数:", corr)
        print("显著性:", pvalue)

        temp["Less相关性系数"]=corrLess
        temp["Less显著性"]=pvalueLess
        result=result.append(temp, ignore_index=True)
result.to_excel("Inattentive 100*5 result.xlsx", index=False)

# print(data_X.describe())
# print(type(data_X.values))


### linear regression ##### Python
#随机选1000次
import pandas as pd
from sklearn.model_selection import KFold
from sklearn.linear_model import LinearRegression
from scipy.stats import pearsonr
import random
##### 每次抽取3个脑区，随机抽取1000次，得到1000个r，每个r是单次抽取进行五折交叉验证的平均r值
def fold5(x, y):
    # 将数据拆成5份
    kf = KFold(n_splits=5)
    x_train_list=[]
    y_train_list=[]
    x_test_list=[]
    y_test_list=[]
    for i, (train_index, test_index) in enumerate(kf.split(x)):
        print("第{}折".format(i))
        # print(train_index)
        # print(test_index)
        # print(x[test_index])
        x_train_list.append(x[train_index])
        y_train_list.append(y[train_index])
        x_test_list.append(x[test_index])
        y_test_list.append(y[test_index])
    return x_train_list, y_train_list, x_test_list, y_test_list

data=pd.read_excel("ADHD_200 patients prediction 2-2.xlsx")
select_cols=data.columns[-70:]
all_X=data[select_cols]

indexList=[]
for i in range(1000): # 这里为了简单写的是随机抽取1000次
    temp=[68, 69] # gender和age肯定在
    for j in range(3):
        temp.append(random.randint(0, 67))
    indexList.append(temp)

xlsx_data=pd.DataFrame()
for item in indexList:
    # print(item)
    X=all_X.values[:, item]
    print(X.shape)
    Y=data["Hyper"].values # 选择Y变量

    X_train_list, Y_train_list, X_test_list, Y_test_list=fold5(X, Y)

    
    all_data_dict=dict()
    # 计算平均显著性，平均相关性
    corr_list=[]
    pvalue_list=[]
    for x_train, y_train, x_test, y_test in zip(X_train_list, Y_train_list, X_test_list, Y_test_list):
        all_data_dict["name"]=",".join(all_X.keys()[x_index] for x_index in item)
        reg = LinearRegression() # 线性回归
        reg.fit(x_train, y_train)
        print("训练系数:\n", reg.coef_) # 输出系数
        # print(reg.predict(x_test).shape)
        # print(y_test.shape)
        corr, pvalue=pearsonr(reg.predict(x_test), y_test)
        print("相关性系数:", corr)
        print("显著性:", pvalue)
        corr_list.append(corr)
        pvalue_list.append(pvalue)
    all_data_dict["平均相关性"]=sum(corr_list)/len(corr_list)
    all_data_dict["平均显著性"]=sum(pvalue_list)/len(pvalue_list)
    xlsx_data=xlsx_data.append(all_data_dict, ignore_index=True)

xlsx_data.to_excel("ADHDHyper结果.xlsx", index=False)

