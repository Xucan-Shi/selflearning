#######################
#第十二步，主成分分析的另一种做法

library(logisticPCA) 
library(ggplot2) 

data("house_votes84")

#######################
#1、对数据进行pca分析 且 确保数据标准化
mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE) 
summary(mtcars.pca) 

#文字表达
#前4个主成分累计解释方差变异性95%

library(devtools) 
install_github("vqv/ggbiplot")
library(ggbiplot) 


#######################
#2、绘制主成分图
ggbiplot(mtcars.pca)

#文字表达
#第一主成分对各变量的解释能力。从该图可以看出，
#第一主成分在各变量上的解释程度相对较强

#增加样本点的标注
ggbiplot(mtcars.pca, labels=rownames(mtcars))


#######################
#3、主成分在某个分类变量中的表现
#如果知道车的产地，可以把汽车放入3个分类，此时可以观察主成分在3个分类中的表现
mtcars = data.frame(mtcars)
dim(mtcars)
mtcars.country <- c(rep("Japan", 3), rep("US",4), 
                    rep("Europe", 7),rep("US",3), "Europe", 
                    rep("Japan", 3), rep("US",4), rep("Europe", 3), 
                    "US", rep("Europe", 3))   #一共32个,32行

#绘图
ggbiplot(mtcars.pca,ellipse=TRUE, 
         labels=rownames(mtcars), 
         groups=mtcars.country)
#从图可以看出，分类为美国的数据分布相对于其他类别有一定的区分度

#查看第3-4主成分的数据分布情况
ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4),labels=rownames(mtcars), 
         groups=mtcars.country)


#######################
#4、利用主成分分析消除变量之间共线性，再实现主成分回归
#首先，实现主成分分析
mtcars.pca <- prcomp(mtcars[,c(2:7,10,11)], center = TRUE,scale. = TRUE) 
summary(mtcars.pca)

#其次，提取前4个主成分，能够解释数据95%的变异性
pca_mtcars <- as.data.frame(mtcars.pca$x[,1:4])
pca_mtcars

#最后，利用前4个主成分与剩余的变量来对mpg做回归
pca_mtcars<-cbind(mtcars[,c(1,8,9)],pca_mtcars)
summary(lm(mpg~.,data=pca_mtcars)) 

#线性回归——便于对比
lm.fit <- lm(mpg~., data = mtcars)
summary(lm.fit)




