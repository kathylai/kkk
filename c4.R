d = read.csv('hmeq.csv',na.strings = '')
View(d)
dc = d[complete.cases(d),]
dim(d)
dim(dc)
sparse matrix 稀疏矩阵

mdist = function(x){ 
  t = as.matrix(x)
  p = dim(t)[2]
  m = apply(t,2,mean)
  s = var(t)
  return(mahalanobis(t,m,s)) }
mdc=mdist(dc[,-c(1,5,6)])#去掉第五第六列文字列 去掉y列‘1’
length(mdc)
#分别对=1的和=0的进行
d1 = dc[dc$BAD==1,]
d0 = dc[dc$BAD==0,]
dim(d1)
dim(d0)
md1=mdist(d1[,-c(1,5,6)])
md0=mdist(d0[,-c(1,5,6)])
c = qchisq(0.99,10)
x1 = d1[md1<c,]
x0 = d0[md0<c,]
x = rbind(x0,x1)
lg.fit = glm(BAD~.,data = x,family = binomial)#逻辑回归
summary(lg.fit$fitted.values)#逻辑回归出的拟合值y值 fitted.values

pred1 = predict(lg.fit,type = 'response')
pred1-lg.fit$fitted.values
pred1[pred1>0.5] <- 1
pred1[pred1<=0.5] <- 0

table(pred1,x$BAD)#对角线是估计正确的 是真值
pred2=pred1
pred2 = predict(lg.fit,type = 'response')
pred2[pred2>0.1]<-1
pred2[pred2<=0.1]<-0
s1=table(pred1,x$BAD)
s2=table(pred2,x$BAD)
error1=(s1[1]+s1[4])/sum(s1)
error2=(s2[1]+s2[4])/sum(s2)