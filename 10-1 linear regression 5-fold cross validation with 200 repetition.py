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
