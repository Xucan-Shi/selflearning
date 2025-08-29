rm(list=ls()) #清空工作环境

data("mtcars")
x <- as.matrix(mtcars[, -1])  # 选取除mpg(第一列)外的所有列作为特征
y <- mtcars$mpg               # 将mpg作为目标变量
mtcars111 = data.frame(mtcars)

#弹性网回归
#1设置alpha的范围：通常，你会选择一个alpha值的向量，这些值覆盖了0到1之间的不同区间。例如，你可以选择alpha = seq(0, 1, by = 0.1)来生成一个从0到1，步长为0.1的alpha值向量。
#2执行交叉验证：使用cv.glmnet函数，并传入你的数据x和y，以及alpha值的向量。函数会对每个alpha值进行交叉验证，并找到每个alpha值下最优的lambda值。
#3选择最优alpha：交叉验证完成后，你可以检查每个alpha值对应的性能度量（如MSE）。通常，你会选择性能度量最优（即MSE最小）的alpha值作为最终模型的参数。
#4训练最终模型：使用选定的最优alpha值和对应的最优lambda值，再次调用glmnet函数来训练你的最终模型。


#可以使用cv.glmnet函数来拟合弹性网络模型。该函数将自动选择最佳的正则化参数。
#拟合弹性网络模型并进行交叉验证







#变量选择——lasso
#预测——ridge
#与lasso相比，Elastic net回归具有组效应，将选择更多相关的变量
library(glmnet)
data(QuickStartExample)
#QuickStartExample111 = data.frame(QuickStartExample)  #原始数据有20个x和1个y
attach(QuickStartExample)

#Lasso(alpha = 1)默认
fit = glmnet(x, y)
plot(fit)
#fit$lambda.min  #此时是求不出lambda的最小值的；必须经过cv
print(fit)

#获取tuning参数取0.1时的参数估计结果
coef(fit,s=0.1)   #系数估计结果

#文字表达
#从该结果可以看出，只有部分变量的系数不为0，可以认为是被选择的变量
class(fit)  

set.seed(29)
nx = matrix(rnorm(10*20),10,20)  #生成200个来自标准正态分布（均值为0，标准差为1）的随机数。
nx
#预测当tuning参数为0.1和0.05时
predict(fit,newx=nx,s=c(0.1,0.05))   #输出的是参数下的预测值，是 nx 中每一行对应的预测值

#如果 fit 是一个分类模型（如逻辑回归，family="binomial"）
#则输出将是每一行对应的类别概率（在二分类情况下）或类别标签（如果指定了 type="class" 参数）。

#要用cross-validation去选择最优的lambda参数
cvfit = cv.glmnet(x, y)
#绘制cross-validation误差变化图
plot(cvfit)
#查看最优取值
cvfit$lambda.min
#查看最优取值时的回归系数
coef(cvfit, s = "lambda.min")

#预测当tuning参数最佳（MSE最小）时
predict(cvfit, newx = x[1:5,], s = "lambda.min")
predict(cvfit, newx = nx, s = "lambda.min")



#设置tuning参数集合
grid <- 10^seq(10, -2, length = 100) #共100个数，10到-2之间
#实现岭回归
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid)
coef(ridge_mod)  #有完整100个lambda的值
plot(ridge_mod)   
dim(coef(ridge_mod))
 
#计算给定tuning参数的岭回归模型回归系数
ridge_mod$lambda[60]
ridge_mod$lambda[100] 
A=coef(ridge_mod)[,60]  #20个变量的回归系数+截距项
A
sqrt(sum(coef(ridge_mod)[-1,60]^2))  #计算惩罚函数的数值；二范式惩罚项的值。


#假设tuning参数=50，重新估计前20个变量的回归系数
predict(ridge_mod, s = 50, type = "coefficients")[1:20,]



#分割数据，50%为测试集、50%为训练集
library(ISLR);library(dplyr);library(tidyr)
Hitters
Hitters = na.omit(Hitters)
set.seed(1)
train = Hitters %>%
  sample_frac(0.5)
test = Hitters %>%
  setdiff(train)
sapply(Hitters, class)

x_train = model.matrix(Salary~., train)[,-1]
x_test = model.matrix(Salary~., test)[,-1]
y_train = train %>%
  dplyr::select(Salary) %>%
  unlist() %>%
  as.numeric()
y_test = test %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()


#thresh = 1e-12设定了一个非常小的阈值（1乘以10的-12次方），意味着任何绝对值小于这个数的系数都会被视作零。
ridge_mod = glmnet(x_train, y_train, alpha=0, lambda = grid, thresh = 1e-12)
ridge_pred = predict(ridge_mod, s = 4, newx = x_test)
ridge_pred  #输出每一行对应的预测值
mean((ridge_pred-y_test)^2)   #平均平方误差MSE

#岭回归的结果依赖于tuning参数，借助cross validation方法来选择最优tuning参数
set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 0, lambda = grid, thresh = 1e-12)
plot(cv.out)  #绘制cv误差随tuning参数变化图
#选择最优的tuning参数
bestlam = cv.out$lambda.min
bestlam





#岭回归\lasso回归\弹性网回归：之前要对数据进行标准化
Hitters1<-scale(Hitters[,-c(14,15,20)],center=T,scale=T)
Hitters1<-cbind(Hitters1,Hitters[,c(14,15,20)])
set.seed(1)
train1 = Hitters1 %>%
  sample_frac(0.5)
test1 = Hitters %>%
  setdiff(train1)
sapply(Hitters1, class)

x_train1 = model.matrix(Salary~., train1)[,-1]
x_test1 = model.matrix(Salary~., test1)[,-1]
y_train1 = train1 %>%
  dplyr::select(Salary) %>%
  unlist() %>%
  as.numeric()
y_test1 = test1 %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()


#thresh = 1e-12设定了一个非常小的阈值（1乘以10的-12次方），意味着任何绝对值小于这个数的系数都会被视作零。
ridge_mod1 = glmnet(x_train1, y_train1, alpha=0, lambda = grid, thresh = 1e-12)
ridge_pred1 = predict(ridge_mod1, s = 4, newx = x_test1)
ridge_pred1  #输出每一行对应的预测值
mean((ridge_pred1-y_test1)^2)   #平均平方误差MSE

#岭回归的结果依赖于tuning参数，借助cross validation方法来选择最优tuning参数
set.seed(1)
cv.out1 = cv.glmnet(x_train1, y_train1, alpha = 0, lambda = grid, thresh = 1e-12)
plot(cv.out1)  #绘制cv误差随tuning参数变化图
#选择最优的tuning参数
bestlam1 = cv.out1$lambda.min
bestlam1




