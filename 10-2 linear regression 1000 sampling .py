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



