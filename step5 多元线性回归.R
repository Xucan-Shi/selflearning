#########################
#第五步，多元线性回归

#回归系数：在其他解释变量保持不变的情况下，每变化1个单位时，Y的均值E(Y)的变化。
#R2表示回归模型可解释部分占总体的百分比
#R2越大，说明模型拟合效果越好，模型的可解释部分被拟合的程度越高

#########################
#1、对数据进行基本描述统计
#汇总分析
summary(rdata)
library(psych)
a=describe(rdata[,c("crime","poverty","single")])
#散点图
pairs(~crime+poverty+single,data=rdata, 
      main="Simple Scatterplot Matrix")


#########################
#2、建立回归模型并估计参数
ols<-lm(crime ~ poverty + single, data = rdata)
anova(ols)
summary(ols)

#文字表达      
#截距项回归系数为-1368.19（t检验显著），
#proverty回归系数为6.79（t检验不显著），
#single回归系数为166.37（t检验显著）

#模型拟合优度为0.71，F检验值为57.96，p值为0.00，说明存在线性模型

#拟合优度为xxx，拟合效果不够好/还可以


#########################
#3、对模型拟合效果进行分析
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
#las = 1总是平行于坐标轴。
#这意味着x轴和y轴的标签都会根据它们各自的轴方向进行调整，使得标签与轴平行。
#如果轴是水平的，标签也是水平的；如果轴是垂直的，标签也是垂直的。

#从图可以看出：数据9、25、51的拟合效果较差
rdata[c(9,25,51),1:2]  #查看拟合效果差的点
#查看拟合效果最差的四个数据
library(MASS)
d1 <- cooks.distance(ols)   #计算偏离程度
#库克距离：库克距离用来判断强影响点是否为Y的异常值点。
#一般认为，当D<0.5时认为不是异常值点；当D>0.5时认为是异常值点
r <- stdres(ols)       #标准化回归误差
a <- cbind(rdata, d1, r)   #绑定在一起，进行分析
a[d1 > 4/51, ]          #前4个数据


#########################
#4、稳健回归拟合方式
#（1）剔除异常值点
#summary(ols <- lm(crime ~ poverty + single, data =a [d1 <= 4/51, ]))
summary(ols<-lm(crime ~ poverty + single, data=rdata[-c(9,25,51), ]))
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)
#看R方和离群点

#稳健回归在剔除离群点或者高杠杆率点和保留离群点
#或高杠杆率点并像最小二乘法那样平等使用各点之间找到了一个折中。

#生成一个absr1变量, 其对应的为残差序列的值，取出残差值较大的观测值
rabs <- abs(r)              #计算标准化回归误差r的绝对值
a <- cbind(rdata, d1, r, rabs)  #绑定到数据集上 
asorted <- a[order(-rabs), ]  #按照-rabs升序，即按照rabs降序对数据进行排序 
asorted[1:10, ] #取前十个数据 


#（2）Huber——用rlm回归
summary(rr.huber <- rlm(crime ~ poverty + single, data = rdata))
#查看加权的情况
hweights<-data.frame(state=rdata$state, resid=rr.huber$resid, weight=rr.huber$w) 
hweights2<-hweights[order(rr.huber$w), ] 
hweights2[1:15, ]
#state是标识或分类变量

#文字表达
#观测值的残差值越大，其被赋予的权重越小。
#哪些权重最小，哪些权重最大 是1。


#（3）Bisquare——用rlm回归
rr.bisquare<-rlm(crime~poverty+single, data=rdata, psi=psi.bisquare) 
summary(rr.bisquare)
#查看加权结果
biweights<-data.frame(state=cdata$state, resid=rr.bisquare$resid, weight=rr.bisquare$w) 
biweights2<-biweights[order(rr.bisquare$w), ]
biweights2[1:15, ]

#文字表达
#与Huber方法相比，bisquare方法下的（state观测行）观测值被赋予了极小的权重。
#能够看到两种方法估计出的回归参数也相差甚大。


#########################
#5、逐步回归选择变量
steplm<-lm(crime~pctmetro+pctwhite+pcths+poverty+single,data=rdata)
step(steplm)
step(steplm,direction = c("both", "backward", "forward"))

#文字表达
#剔除pcths变量，会降低AIC
#剔除pchwhite变量，会降低AIC
#没有可以剔除的变量

#模型拟合优度为xxx，拟合效果好
#通过逐步回归从可供选择的所有自变量中选出了对因变量有显著影响的变量
#完成了选择自变量建立最优回归方程的目的。


