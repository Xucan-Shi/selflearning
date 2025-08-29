#######################
#第三步，描述统计（表格）
#####################
x = c(1,2,3,4,5,10)
#表格
summary(mydata)
#或
fivenum(mydata)
#或
describe(mydata)
#或
library(pastecs)
stat.desc(x,basic=TRUE,desc=TRUE,norm=FALSE,p=0.95)

#文字描述
#（从描述统计的结果可以看出，自行车库存数据一共有24个样本，-个缺失，
#购买价格的最小值为15美元，最大值为490.50美元，中位数为194.22美元，
#均值为232.03美元，说明不完全是正态分布，数据偏左）



#####################
#分组统计
library(psych)
describeBy(X, group=3)  #group = 3 就是对第三列进行分类并作描述统计

#或
describeBy(newdata, newdata$Gender)

#或
#分类汇总分析，对于连续型变量
summary(mydata[admit==1,])
summary(mydata[admit==0,])


#文字描述
#从描述统计的结果可以看出，样本量中，有135个是女性，290个是男性，
#其中女性的存款范围为0-19568，中位数为680，均值为2075.08（3916.85）；
#男性的存款范围为0-19811，中位数为558.5，均值为1690.36(3438.70) ，
#从均值的数量来看，女性的存款量相对男性高。



#####################
#频数统计——交叉表
FData <- c(6,7,5,7,7,8,7,6,9,7,4,10,6,8,8,9,5,6,4,8)
a<-table(FData)  #产生频数  table(x1,x2) ftable(x1,x2,x3) #按行
prop.table(a)  #产生比例
margin.table(a, 1) #统计交叉表的边际分布，按行统计，即统计按照A分组的频数表
margin.table(a, 2)

#制作交叉表用xtabs或者CrossTable——也叫频数表
mytable<-xtabs(~x1+x2+x3, data=X)
ftable(mytable)
summary(mytable) #进行卡方检验
#或
library(gmodels)
attach(X)
CrossTable(x1,x2)
detach(X)

#文字描述
#表5可以看出，女性中有78位信用水平较高，占58%，57位信用水平较低，占42%；
#男性中有133位信用水平较高，占46%，157位风险水平较低，占54%。
#结果表明，女性信用水平高的比例相对于男性的比例较高。

