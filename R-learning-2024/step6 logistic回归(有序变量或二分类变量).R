#########################
#第六步，logistic回归
#因变量——分类（二分类或多分类）
#自变量随意

#文字表达
#β0（常数项）：
#不考虑解释变量的情况下，事件y发生的概率与不发生的概率之比的自然对数。 

#自变量是二分类
#OR优势比：表示，解释变量Xi发生与不发生时，事件y发生的odds之比
#解释变量X=1时（暴露在因素X下），会促进事件Y的发生率提升
#解释变量X=0时（不暴露在因素X下），会抑制事件Y的发生率的提升

#自变量是等级（定序），其对应的回归系数的意义：
#每相差一个等级时的比数比的对数，而exp(β)则表示每增加一个等级时的比数比值。

#自变量连续变量
#x每增加一单位时比数比的对数。exp(β)表示年龄增加一单位时比数比值。

OR=exp(0.786)=2.195
#服用口服OC者发生心肌梗塞的概率是不服者的2.195倍。

#########################
#1、分类描述统计
summary(mydata[admit==1,])
summary(mydata[admit==0,])
xtabs(~admit + rank, data = mydata)  #全部是分类变量


#########################
#2、对等级变量因子化（只要不是二分类的分类变量，都因子化）
mydata$rank <- factor(mydata$rank) 


#########################
#3、建立Logistic回归模型
logit.glm<-glm(y~x1+x2+x3,family=binomial,data=d1)  #binomial二项分布；poission泊松分布
summary(logit.glm)

#文字表达
#GRE成绩每增加1分，被录取的优势对数(log odds)增加0.002
#而GPA每增加1单位，被录取的优势对数(log odds)增加0.804。
#最后第二名的同学比第一名同学在其它同等条件下被录取的优势对数(log odds)小了0.675
#看来排名非常重要


#########################
#4、逐步回归
logit.step<-step(logit.glm,direction="both")
summary(logit.step) 


#########################
#5、置信区间+怀特检验——如果某自变量有多个水平的话
#参数的置信区间
confint.default(mylogit)
#对参数rank的怀特检验
wald.test(b=coef(mylogit), Sigma=vcov(mylogit), Terms = 4:6) #估计参数所在的行

#文字表达
#模型显著拒绝原假设，说明rank多个水平之间存在显著的统计学差异（有序变量）


#########################
#6、输出结果
exp(coef(mylogit))
exp(cbind(OR=coef(mylogit), confint(mylogit)))


#########################
#7、预测发生y=1的概率
#相当于分类做回归
pre1<-predict(logit.step, data.frame(x1=1))  #预测视力正常司机Logistic回归结果
p1<-exp(pre1)/(1+exp(pre1))  #预测视力正常司机发生事故概率

pre2<-predict(logit.step,data.frame(x1=0))  #预测视力有问题的司机Logistic回归结果
p2<-exp(pre2)/(1+exp(pre2))  #预测视力有问题的司机发生事故概率
c(p1,p2)  #结果显示

#或
#准备需要预测的数据
newdata1<-with(mydata, data.frame(gre=mean(gre), gpa=mean(gpa), rank=factor(1:4)))
#预测
#直接返回预测的概率值，也就是录取的概率
newdata1$rankP<-predict(mylogit, newdata=newdata1, type="response") 
newdata1

log(0.5166016/(1-0.5166016))  #优势对数 或者说 比数比

#或
r<-predict(mylogit, newdata=newdata1)   #比数比
r


#########################
#8、画图——不同的rank下，gre增长对录取的影响
#构造绘图数据
newdata2<-with(mydata,
                 data.frame(gre=rep(seq(from=200, to=800, length.out=100), 4),
                            gpa=mean(gpa), rank=factor(rep(1:4, each = 100))))
#length.out=100：指定生成的序列长度是100个数值（序列恰好包含100个数值）

newdata3<-cbind(newdata2,predict(mylogit,newdata=newdata2,type="link",se=TRUE)) 
#type="link"给出logit线性函数的预测值

newdata3<-within(newdata3, { 
  PredictedProb <- plogis(fit) 
  LL <- plogis(fit - (1.96 * se.fit)) 
  UL <- plogis(fit + (1.96 * se.fit)) 
})
#plogis会将预测值处理成概率值

#绘制图形
ggplot(newdata3,aes(x=gre, y=PredictedProb))+geom_ribbon(aes(ymin=LL, ymax=UL, fill=rank), alpha=.2)+geom_line(aes(colour=rank), size=1)

