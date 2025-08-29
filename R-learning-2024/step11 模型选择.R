#######################
#第十一步，模型选择
#需要提前标准化数据

#######################
#1、标准化数据
#对所有数值型变量标准化
Hitters1<-scale(Hitters[,-c(14,15,20)],center=T,scale=T)
Hitters<-cbind(Hitters1,Hitters[,c(14,15,20)])
class(Hitters) #查看数据类型（dataframe 或 list等）

#######################
#2、分割数据
Hitters = na.omit(Hitters)
set.seed(1)
train = Hitters %>%
  sample_frac(0.5)
test = Hitters %>%
  setdiff(train)

sapply(Hitters, class) #查看数据格式
x_train = model.matrix(Salary~., train)[,-1] #除了因变量外所有变量
x_test = model.matrix(Salary~., test)[,-1]
y_train = train %>%
  dplyr::select(Salary) %>%
  unlist() %>%
  as.numeric()
y_test = test %>%
  dplyr::select(Salary) %>%
  unlist() %>%
  as.numeric()

#设置lambda参数集合，便于后续使用
grid <- 10^seq(10, -2, length = 100)


#######################
#3、岭回归
#thresh = 1e-12设定了一个非常小的阈值（1乘以10的-12次方）
#意味着任何绝对值小于这个数的系数都会被视作零。
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

#################
#4、计算测试误差
#把选择的参数放到模型里，去计算测试集的预测误差
#首先，用test来测试模型——岭回归，利用cv选择最优lambda进行测试
ridge_pred = predict(ridge_mod, s = cv.out$lambda.min, newx = x_test)
#计算平方损失
mean((ridge_pred-y_test)^2)


#######################
#5、Lasso回归
#用train来训练模型——此处采用Lasso回归
lasso_mod = glmnet(x_train, y_train, alpha=1, thresh = 1e-12)
set.seed(1)
cv.out_lasso = cv.glmnet(x_train, y_train, alpha = 1)  #默认nfolds = 10
plot(cv.out_lasso)  #绘制cv误差随tuning参数变化图

#用test来测试模型——lasso回归，利用cv选择最优lambda进行测试
lasso_pred = predict(lasso_mod, s = cv.out_lasso$lambda.min, newx = x_test)
#计算平方损失
mean((lasso_pred - y_test)^2)


#######################
#6、弹性网回归
#（1）设置alpha的范围
alpha_values <- seq(0, 1, by = 0.1)

#（2）执行交叉验证
# 使用cv.glmnet函数进行交叉验证，family参数根据y的类型选择（这里是线性回归，所以选择"gaussian"）
cv_fits <- lapply(alpha_values, function(alpha) {
  cv_fit <- cv.glmnet(x_train, y_train, alpha = alpha)
  return(cv_fit)
})

#（3）选择最优alpha
#提取每个alpha值对应的MSE（或其他性能度量）
mse_values <- sapply(cv_fits, function(cv_fit) {
  return(min(cv_fit$cvm)) # cvm是交叉验证的均方误差
})
# 找到MSE最小的alpha值
optimal_alpha <- alpha_values[which.min(mse_values)]

#optimal_lambda <- cv_fits[[which.min(mse_values)]]$lambda.min # 对应的最优lambda值

# 打印最优alpha和lambda值
cat("Optimal alpha:", optimal_alpha, "\n")

#cat("Optimal lambda:", optimal_lambda, "\n")

cv.out = cv.glmnet(x_train, y_train, alpha = optimal_alpha)
plot(cv.out) 
bestlam = cv.out$lambda.min
bestlam

#（4）训练最终模型
final_model <- glmnet(x_train, y_train, alpha = optimal_alpha, lambda = bestlam)
coef(final_model)

#用test来测试模型——enet回归，利用cv选择最优lambda进行测试
enet_pred = predict(final_model, s = bestlam, newx = x_test)
#计算平方损失
mean((enet_pred - y_test)^2)



#######################
#7、弹性网回归(逐一)
grid <- 10^seq(10, -2, length = 100)
#反复进行如下的一段操作即可
cv_fit <- cv.glmnet(x_train, y_train, alpha = 0.1,lambda = grid)
bestlam = cv_fit $lambda.min
best_model <- glmnet(x_train, y_train, alpha = 0.1, lambda = bestlam)
enet_pred = predict(best_model, s = bestlam, newx = x_test)
mean((enet_pred - y_test)^2)




