#######################
#第九步，主成分回归
#尝试，验证是否需要提前标准化数据——结论：不需要！！！
X = read.table("clipboard",header = T)
cor(X)
#princomp() 函数在默认情况下 不会 自动对数据进行标准化处理。
PCA=princomp(X,cor=T)  #主成分分析
PCA     

#标准化数据
scaled_X<-scale(X)
scaled_X<-data.frame(scaled_X)
PCA_scale=princomp(scaled_X,cor=T)  #主成分分析
PCA_scale   
summary(PCA)
summary(PCA_scale)
PCA$loadings
PCA_scale$loadings
#但是两种的pca结果是完全一样的；也就是说，不提前进行标准化也可以。

#######################
#1、相关系数矩阵
cor(data)
#或

#先用线性回归的做法来进行回归分析
lm.sol<-lm(data$y1~x1+x2+x3+x4, data=data)
summary(lm.sol)
#发现结果不好

#######################
#2、主成分分析
PCA=princomp(data,cor=T)
PCA 
#主成分载荷
PCA$loadings

#######################
#3、确定主成分
screeplot(PCA,type="lines")

#######################
#4、主成分得分
#选中的主成分
PCA$scores[,1:2]
#画图
biplot(PCA)

#######################
#5、计算主成分与原变量之间的相关系数
pre<-predict(PCA)   #将原始的数据转换成用主成分变量来表达
data1<-cbind(data,pre)
cor(data1)  #相关系数

data$z1<-pre[,1]
data$z2<-pre[,2]  #把主成分的值放入到最开始的dataframe中

#文字表达
#先计算样本的主成分的预测值
#并将第1主成分的预测值和第2主成分的预测值存放

#######################
#6、主成分回归
lm.sol<-lm(y1~z1+z2, data=data)
summary(lm.sol)

#文字表达
#这是因变量与主成分的关系，应用起来不方便，需变换成因变量与自变量的关系
#截距项回归系数为1.035873（t检验显著）
#z1回归系数为-0.037110（t检验显著）
#z2回归系数为-0.013328（t检验不显著）

#######################
#7、变换主成分为原自变量，得到因变量与自变量的关系
beta<-coef(lm.sol)       #提取回归系数
A<-loadings(PCA)     #提取主成分对应的特征向量
x.m<-PCA$center        #数据的中心,即各类数据均值
x.sd<-PCA$scale       #数据的标准差         
coef<-(beta[2]*A[,1]+beta[3]*A[,2])/x.sd
beta0<-beta[1]-sum(x.m*coef)
c(beta0,coef)



