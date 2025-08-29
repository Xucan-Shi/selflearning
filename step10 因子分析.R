rm(list=ls()) #清空工作环境
library(devtools)
library(ggplot2)
library(readxl);library(Hmisc);library(psych);
library(pastecs);library(gmodels);library(MASS)
library(mgcv);library(dplyr);library(tidyverse)
library(foreign);library(nnet);library(reshape2);library(sandwich)
library(lmtest)
library(aod)
source("mvstats.R")   #在“复习内容”里

#######################
X=read.table("clipboard",header=T)
cor(X)
#文字表达
#主营业务利润率x1与销售毛利率x2呈高度正相关，
#速动比率x3与资产负债率x4呈较强的负相关。
#主营业务收入增长率x5和营业利润增长率x6呈中度相关关系。
#为了消除各财务指标之间的相关性，采用因子分析方法并提取因子。


#######################
#计算特征值、因子载荷及共同度 
#极大似然法因子分析
FA0=factanal(X,3,rot="none")
FA0
#主成分法因子分析
Fac=factpc(X,3,rot="none")
Fac


#######################
#使用最大方差正交旋转法
#加入rotation旋转因子
#极大似然法因子分析
FA1=factanal(X,3,rot="varimax")
FA1
#主成分法因子分析
Fac1=factpc(X,3,rot="varimax")
Fac1   #factpc有rot参数

#旋转后各因子代表的经济意义十分明显。


#######################
#计算因子得分
#极大似然法因子分析——因子得分
Fa1=factanal(X,3,scores="regression") #已经默认旋转了
Fa1$scores
#可以得到每行数据，即每个观测数据的因子情况

Fa2=factanal(X,3,rot = "varimax", scores="regression")  #测试一下是否旋转
Fa2$scores

#主成分法因子分析——因子得分
Fac1=factpc(X,3,scores="regression") 
Fac1$scores

