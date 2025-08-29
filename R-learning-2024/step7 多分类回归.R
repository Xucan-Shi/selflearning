#######################
#第七步，多分类回归


#######################
#1、描述统计
with(data,table(ses,prog)) 
with(data,do.call(rbind,tapply(write,prog,function(x) c(M=mean(x), SD=sd(x)))))


#######################
#2、设定参照类，做多分类logistic回归分析
data$prog2<-relevel(data$prog, ref="academic")
#多分类回归
test<-multinom(prog2 ~ ses + write, data = data)

#一定记住是 参照类~x1+x2  x1可以是有序变量

#文字表达
#判断参数估计过程是否收敛


#######################
#3、获取多分类logistic回归分析结果
summary(test)

#计算标准化得分 z-score及p-value
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z),0,1))*2 
p
#(pnorm是正态分布的分布函数值
#pnorm（q,mean=0,se=1)
#dnorm:正态分布概率密度函数
#rnorm：构造正态分布随机数)

#提取参数估计值并转换为数值
exp(coef(test))

#文字表达
#从p值来看，xxx变量是显著的
#“ses”社会地位变量从1变为3时，普通项目vs学术项目的相对优势比是0.3126
#Write变量增加一个单位，普通项目vs学术项目的相对优势比是0.9437

#查看各样本所属分类的概率
#使用fitted函数来得到模型的估计值
head(pp <- fitted(test))


#######################
#4、预测：Write不变，检测社会地位的改变对预测值的影响
dses<-data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test, newdata = dses, "probs")

#不同write分数，检测社会地位的改变对预测值的影响
#准备数据，包含3个样本
dwrite<-data.frame(ses=rep(c("low", "middle", "high"), each=41) , write=rep(c(30:70),3))
#预测，并把预测结果与dwrite结合起来
pp.write<-cbind(dwrite, predict(test, newdata=dwrite, type="probs", se=TRUE))
#按分类对3-5列（预测值）求列均值
by(pp.write[,3:5], pp.write$ses, colMeans)


#######################
#5、绘图：绘制在不同社会地位下，预测值与写作分数的关系图
#转换数据为长数据
lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")

(variable.name = "Variable") #——是academic、vocational等 y的分类
#相当于把有些列（分类型的）转为行

#绘制图形
ggplot(lpp,aes(x=write, y=probability, colour=ses))+geom_line()+facet_grid(variable ~ ., scales = "free")
#variable ~ . 的意思是按照 variable 这个变量的不同水平来创建行（rows）
#"free" 意味着每个面板可以有不同的 x 轴和 y 轴范围，
#这通常用于当不同面板的数据范围差异较大时，以便更好地展示每个面板内的数据。


