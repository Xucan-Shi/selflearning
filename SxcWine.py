import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.formula.api import ols, glm

red_wine = pd.read_csv('data/winequality-red.csv', sep=';', header=0)
white_wine = pd.read_csv('data/winequality-white.csv', sep=';', header=0)
red_wine['type']="r"
white_wine['type']="w"
wine = pd.concat([red_wine, white_wine], axis=0, ignore_index=True) #避免重复出现的红白酒index
wine.columns = wine.columns.str.replace(' ','_')
print(wine.head())
#一、描述性统计
print(wine.describe())
print(sorted(wine.quality.unique())) #取唯一值（一共有几个值）
print(wine.quality.value_counts()) #数一下每个值有多少条观测

#二、分组、直方图与检验
print(wine.groupby('type')[['quality']].describe().unstack('type')) #按照类型显示质量的描述性统计
print(wine.groupby('type')[['quality']].quantile([0.25,0.75]).unstack('type'))

r_wine = wine.loc[wine['type']=='r', 'quality'] #定位找出行为red，质量为列的
print(r_wine.head(10))
w_wine = wine.loc[wine['type']=='w', 'quality'] #定位找出行为white，质量为列的
sns.set_style("darkgrid") #设置背景


print(sns.distplot(r_wine, norm_hist = True, \
                  kde = False, color='red', label="Red wine"))
print(sns.histplot(r_wine, \
                  kde = False, color='red', label="Red wine"))
print(sns.histplot(w_wine, \
                  kde = False, color='white', label="White wine"))
sns.utils.axlabel("Quality Score", "Density")
plt.title("Distribution of Quality by Wine Type")
plt.legend()
plt.show()

print(wine.groupby(['type'])[['quality']].agg(['std'])) #检验红酒白酒质量的标准差是否相同——平均质量
tstat, pvalue, df = sm.stats.ttest_ind(r_wine, w_wine) #进行t检验
print('tstat: %.3f pvalue: %.4f' % (tstat, pvalue)) #输出t值和pvalue

#三、成对变量之间的关系和相关性
print(wine[wine.columns.difference(['type'])].corr())
def take_sample(data_frame, replace=False, n=200):
    return data_frame.loc[np.random.choice(data_frame.index,\
                                           replace=replace, size=n)] #小样本，随机取一个行的子集
reds_sample = take_sample(wine.loc[wine['type']=='r',:])
whites_sample = take_sample(wine.loc[wine['type']=='w',:])
wine_sample = pd.concat([reds_sample, whites_sample])

print(wine_sample[wine_sample.columns.difference(['type'])].corr())
wine['in_sample'] = np.where(wine.index.isin(wine_sample.index),1.,0.)
print(pd.crosstab(wine.in_sample, wine.type, margins=True)) #交叉表：行是in_sample的，列是品种，即统计不同种类的insample数量

sns.set_style("dark")
g =sns.pairplot(wine_sample, kind='reg', diag_kind='hist', plot_kws={"ci":False,\
            "x_jitter":0.25, "y_jitter": 0.25}, hue='type',\
                diag_kws={"bins": 10, "alpha":1.0}, palette=dict(r="red",w="white"),\
                    markers=["o","s"], vars=['quality', 'alcohol', 'residual_sugar'])
print(g) #查看成对变量之间的关系
plt.suptitle('Histograms and Scatter Plots of Quality, Alcohol, and Residual Sugar',\
             fontsize=14, horizontalalignment='center', verticalalignment='top',\
                x=0.5, y=1)
plt.show()

#四、使用最小二乘估计进行线性回归
my_formula = 'quality~alcohol + chlorides + citric_acid + density\
    + fixed_acidity + free_sulfur_dioxide + pH + residual_sugar + sulphates\
        + total_sulfur_dioxide + volatile_acidity'  #将一个字符串赋给变量 my_foumula，相当于我自己的公式
lm = ols(my_formula, data=wine).fit()

#输出回归结果
print(lm.summary())
print("\nQuantities you can extract from the result:\n%s" % dir(lm))
print("\nCoefficients:\n%s" % lm.params)
print("\nCoefficient Std Errors:\n%s" % lm.bse)
print("\nAdj. R-squared:\n%.2f" % lm.rsquared_adj)
print("\nF-statistic: %.1f P-value: %.2f" % (lm.fvalue, lm.f_pvalue))
print("\nNumber of obs: %d Number of fitted values: %d" % (lm.nobs,\
                                                           len(lm.fittedvalues)))
#自变量标准化（因为自变量测量单位相差大）
dependent_variable = wine['quality'] #创建一个名为dependent_variable的序列来保存质量数据
independent_variables = wine[wine.columns.difference(['quality','type','in_sample'])]
#进行标准化
independent_variables_standardized = (independent_variables - independent_variables.mean())/independent_variables.std()
wine_standardized = pd.concat([dependent_variable, independent_variables_standardized], axis = 1)
print(wine_standardized.head(15))
#重新对标准化后的wine_standardized进行ols回归
lm_standardized = ols(my_formula, data = wine_standardized).fit()
print(lm_standardized.summary())

#五、预测
red_count=int(red_wine['type'].count() / 2)
white_count=int(white_wine['type'].count() / 2)
print(red_count)
red_sample = take_sample(wine.loc[wine['type']=='r',:],False,red_count)
print(reds_sample.count())
white_sample = take_sample(wine.loc[wine['type']=='r',:],False,white_count)
observations=pd.concat(red_sample,white_sample,axis=0)
y_predicted=lm.predict(observations)
y_predicted_rounded = [round(score, 2) for score in y_predicted]





