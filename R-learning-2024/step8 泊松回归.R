#######################
#第八步，泊松回归
#因变量计数变量 + 自变量数值或分类
#log(因变量)与自变量呈现线性关系
#模型使用前需要检验期望与方差是否相等。(各自变量水平上的因变量)

#######################
#1、检查data.frame中所有列的数据类型
df <- data.frame(
  A = factor(c("a", "b", "c")),
  B = c(1, 2, 3),
  C = c("vocational","general","academic"),
  D = c(T,F,T)
)
class(df[["A"]])  #factor
class(df[["B"]])  #numeric
class(df[["C"]])  #character
class(df[["D"]])  #logical
# 检查data.frame中所有列的数据类型
sapply(df, class)

#把不是因子的需要用的分类变量转为因子
p <- within(p, { 
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)})


#######################
#2、描述性统计
summary(p)
#对num_awards按照prog分类求均值和标准差
with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))       
}))
#发现均值不等于标准差，没关系

ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")    #频数统计


#######################
#3、建立回归模型
summary(m1<-glm(num_awards ~ prog + math,family="poisson",data=p))


#######################
#4、计算p值及置信区间
#方差不等于均值——type="HC0"解决。
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate = coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2*pnorm(abs(coef(m1)/std.err),lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est



#######################
#5、模型的拟合优度，通过卡方统计量来判断模型的拟合优度
with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#残差偏差、残差自由度、p值
#（如果p值很小，观测数据与模型预测之间的差异在统计上显著）
#模型与数据的拟合相对较好


#######################
#6、预测
p$phat<-predict(m1, type="response")   #直接输出预测值

#按照program先排列再按数学分数排列
p<-p[with(p, order(prog, math)), ]

#图形
ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")

position=position_jitter(h=.2)#h水平抖动；w=.2垂直抖动；便于看清点的分布，尽量不重合


