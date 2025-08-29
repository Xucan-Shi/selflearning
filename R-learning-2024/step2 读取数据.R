#########################
#第二步，读取数据

#复制
X=read.table("clipboard",header=T) #第一列会读到索引里 #可以读文本文件
#或者
b<-read.delim("clipboard") #第一列不会

#excel
X<-read_excel("xxx.xlsx")

#csv
X<-read.csv("textdata.csv") 

#手动录入
x1<-c(1,2,3,4,5)
x2<-c(10,19,18,16,15)
X<-data.frame(x1,x2)
#或者
X<-data.frame(
  name=c("amy","james","jeffrey","john"),
  sex=c("F","M","F","M"),      
  age=c(13,12,13,12),
  height=c(56.5,57.3,62.5,59.0),
  weight=c(84.0,83.0,84.8,99.5))
