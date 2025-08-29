#########################
#数据标准化

data = data(iris)
head(iris)

iris[1:4] #1-4列
iris[0:2] #前两列
# 使用is.na()来检查NA和NaN，并移除包含它们的行
Hitters_clean <- Hitters[!apply(Hitters, 1, any), ]

Standard0<-scale(iris[1:4]) #默认做均值为0，标准差为1的标准化。
head(Standard0)

Standard1<-scale(iris[1:4],center=T)   #结果与Standard0一样
head(Standard1)

Standard2<-scale(iris[1:4],center=F,scale=T)  #结果不一样
head(Standard2)

Standard3<-scale(iris[1:4],center=T,scale=T)  #结果与Standard0一样
head(Standard3)
#中心化：数据集中的各项数据减去数据集的均值。
#标准化：在中心化之后再除以标准差。变换后值域为[0,1]。












