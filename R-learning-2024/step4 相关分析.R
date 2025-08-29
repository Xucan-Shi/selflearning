#########################
#第四步，相关分析
#｜r｜<0.3，视为不相关；
#0.3～0.5属于低度相关；
#0.5～0.8属于显著相关，
#0.8以上属高度相关
x<-c(171,175,159,155,152,158,154,164,168,166,159,164)
y<-c(57,64,41,38,35,44,41,51,57,49,47,46)

#########################
#1、先plot出x和y的散点图
plot(x,y)
# 散点图
pairs(~crime+poverty+single,data=rdata,main="Simple Scatterplot Matrix")


#########################
#2、建立离均差乘积和函数；计算相关系数
lxy<-function(x,y){sum(x*y)-sum(x)*sum(y)/length(x)}

r=lxy(x,y)/sqrt(lxy(x,x)*lxy(y,y))
r



#########################
#3、相关系数的假设检验
n=length(x)#向量的长度
tr=r/sqrt((1-r^2)/(n-2))#相关系数假设检验t统计量
tr  #t值比较大，更容易拒绝原假设，显著。

#或
cor.test(x,y,altertnative=c("two.side","less","greater"),method=c("pearson","kendall","spearman"))

#pearson用于2变量正态分布

#kendall用于2变量特定属性排序

#spearman用于2变量不符合正态分布——排序，等级数据
cor.test(x,y)

#文字表达
#由于p = xxx<0.05，于是在显著性水平为0.05的水准上拒绝原假设
#可以认为人群身高与体重呈现正的线性关系，且相关程度 高、中、低。

