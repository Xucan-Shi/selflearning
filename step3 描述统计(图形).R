#######################
#第三步，描述统计（图形）
#####################
#图形


#####################
#条形图geom_bar
#适合x是分类变量，y是数值或者频数
#1个分类+1个数值
ggplot(data)+geom_bar(aes(x=Time,y=demand)) #y是频数 #只给x，就是x的频数
ggplot(data)+geom_bar(aes(x=Time,y=demand),stat="identity")  #y是变量值
#若x有空缺值想避免作图有空列
#或者x是分类变量
data$Time<-factor(data$Time) #先改为因子


#####################
#簇状条形图和堆积面积图geom_bar
#2个分类变量+1个数值
#一个类在x，一个类在fill，用颜色区分
ggplot(cabbage_exp)+geom_bar(aes(x=Cultivar,y=Weight, fill=date), 
                             stat="identity", position="dodge")
#color改边框或线的颜色，fill改面积颜色；fill一般fill类别



#####################
#饼图geom_bar+coord_polar
#1个分类变量（频数）
ggplot(mydata,aes(x=factor(1),fill=Loan.Purpose))+geom_bar()+coord_polar(theta="y")
#极坐标；fill一般fill类别



#####################
#散点图geom_point
#2个数值
ggplot(mtcars, aes(x=wt,y=mpg))+geom_point(color="blue")



#####################
#折线图geom_line
#2个数值
ggplot(mtcars, aes(x=wt,y=mpg))+geom_line()+geom_point()
#2个数值+分类；分类用group
ggplot(mtcars, aes(x=wt,y=mpg,group=factor(gear)))+geom_line(aes(color=factor(gear)))
#平滑曲线+stat_smooth()
ggplot(mtcars, aes(x=wt,y=mpg))+geom_point()+stat_smooth()
ggplot(mtcars, aes(x=wt,y=mpg))+geom_point()+stat_smooth(method=lm)
#填充颜色
ggplot(mtcars, aes(x=wt,y=mpg))+geom_point()+stat_smooth(method=lm,fill="red",size=2,alpha=0.5,color="green")
#分组拟合，不用group也可以，直接fill
ggplot(mtcars, aes(x=wt,y=mpg))+geom_point()+stat_smooth(method=lm,aes(color=factor(cyl),fill=factor(cyl)))+geom_point(aes(color=factor(cyl))) 



#####################
#频数直方图geom_histogram
#1个数值
#先划分组，再画图
binsize<-diff(range((mydata$Age)))/20 
ggplot(mydata,aes(x=Age))+geom_histogram(binwidth=binsize, fill="pink", colour="blue")




#####################
#频数直方图geom_density
#1个数值
ggplot(mtcars,aes(x=wt))+geom_density()

#概率直方图+概率密度曲线geom_histogram + geom_density + stat_function
binsize<-diff(range((mydata$Age)))/20
ggplot(mydata,aes(x=Age)) + 
  geom_histogram(aes(y=..density..),binwidth = binsize, fill="pink", colour="blue")+ 
  stat_function(fun=dnorm, args=list(mean(mydata$Age),sd(mydata$Age)), colour="red", size=1) +  #正态分布拟合曲线
  geom_density(colour="blue",size=1)

fun=dnorm #生成以xxx为均值、xxx为标准差的正态分布



#####################
#面积图geom_area
#1个分类变量+2个数值变量
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup))+geom_area()



#####################
#分面图facet_grid或者facet_wrap
#1个分类变量+2个数值变量
ggplot(uspopage,aes(x=Year,y=Thousands))+geom_area()+facet_wrap(~AgeGroup)



#####################
#等高图stat_density2d + geom_point
#2个数值变量
ggplot(mtcars,aes(x=wt,y=mpg,color="red"))+geom_point(size=2)+stat_density2d()








