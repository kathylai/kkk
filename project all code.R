#1.数据导入
d=read.csv("train.csv",header = TRUE)
dc=d[complete.cases(d),]
hist(d$y)

#2.分类
d0=d[d$y==0,]
d1=d[d$y==1,]
d2=d[d$y==2,]
d3=d[d$y==3,]

#3.贴标签
label0=sample(c(1:10),dim(d0[1]),replace=TRUE)
label1=sample(c(1:10),dim(d1[1]),replace=TRUE)
label2=sample(c(1:10),dim(d2[1]),replace=TRUE)
label3=sample(c(1:10),dim(d3[1]),replace=TRUE)


#4.分测试训练
errorate_pred_log=matrix(rep(0,10),nrow=10)
for (i in 1:9)
{
  d0_train=d0[label0<=i,]
  d0_test=d0[label0>i,]
  d1_train=d1[label1<=i,]
  d1_test=d1[label1>i,]
  d2_train=d2[label2<=i,]
  d2_test=d2[label2>i,]
  d3_train=d3[label3<=i,]
  d3_test=d3[label3>i,]
  
  #5.合并训练集和测试集
  d_train=rbind(d0_train,d1_train,d2_train,d3_train)
  d_test=rbind(d0_test,d1_test,d2_test,d3_test)
  
  #6.分析方法
  #6-1.逻辑回归
  library(nnet)
  re_log=multinom(y~.,data=d_train)
  pred_log=predict(re_log,newdata=d_test)
  tab_log=table(d_test$y,pred_log)
  errorate_pred_log[i,]=(1-sum(diag(tab_log))/sum(tab_log))*100
}
min_pred_log = which.min(errorate_pred_log[which(rowSums(errorate_pred_log)>0),])
pred_log_v=c(errorate_pred_log[min_pred_log])

#6-2.id3
library(rpart)
errorate_pred_id3 =matrix(rep(0,10),nrow=10)
for (i in 1:9)
{
  d0_train=d0[label0<=i,]
  d0_test=d0[label0>i,]
  d1_train=d1[label1<=i,]
  d1_test=d1[label1>i,]
  d2_train=d2[label2<=i,]
  d2_test=d2[label2>i,]
  d3_train=d3[label3<=i,]
  d3_test=d3[label3>i,]
  
  d_train=rbind(d0_train,d1_train,d2_train,d3_train)
  d_test=rbind(d0_test,d1_test,d2_test,d3_test)
  
  
  re_id3 = rpart(y~.,data = d_train,method = "class",parms = list(split="information"),control = rpart.control(cp=0.001))
  min = which.min(re_id3$cptable[,4])
  re_id3_f = prune(re_id3,cp=re_id3$cptable[min,1])
  pred_id3 = predict(re_id3_f,newdata = d_test,type = "class")
  tab_id3 = table(d_test$y,pred_id3)
  errorate_pred_id3[i,]=(1-sum(diag(tab_id3))/sum(tab_id3))*100
}
min_pred_id3 = which.min(errorate_pred_id3[which(rowSums(errorate_pred_id3)>0),])
pred_id3_v=c(errorate_pred_id3[min_pred_id3])

#6-3.CART
library(rpart)
errorate_pred_CART =matrix(rep(0,10),nrow=10)
for (i in 1:9)
{
  d0_train=d0[label0<=i,]
  d0_test=d0[label0>i,]
  d1_train=d1[label1<=i,]
  d1_test=d1[label1>i,]
  d2_train=d2[label2<=i,]
  d2_test=d2[label2>i,]
  d3_train=d3[label3<=i,]
  d3_test=d3[label3>i,]
  
  d_train=rbind(d0_train,d1_train,d2_train,d3_train)
  d_test=rbind(d0_test,d1_test,d2_test,d3_test)
  
  
  re_CART = rpart(y~.,data = d_train,method = "class",parms = list(split="gini"),control = rpart.control(cp=0.001))
  min = which.min(re_CART$cptable[,4])
  re_CART_f = prune(re_CART,cp=re_CART$cptable[min,1])
  pred_CART = predict(re_CART_f,newdata = d_test,type = "class")
  tab_CART = table(d_test$y,pred_CART)
  errorate_pred_CART[i,]=(1-sum(diag(tab_CART))/sum(tab_CART))*100
}
min_pred_CART = which.min(errorate_pred_CART[which(rowSums(errorate_pred_CART)>0),])
pred_CART_v=c(errorate_pred_CART[min_pred_CART])

#6-4.RANDOMFOREST
install.packages("randomForest")
library("randomForest")
errorate_pred_rf =matrix(rep(0,10),nrow=10)
for (i in 1:9)
{
  d0_train=d0[label0<=i,]
  d0_test=d0[label0>i,]
  d1_train=d1[label1<=i,]
  d1_test=d1[label1>i,]
  d2_train=d2[label2<=i,]
  d2_test=d2[label2>i,]
  d3_train=d3[label3<=i,]
  d3_test=d3[label3>i,]
  
  d_train=rbind(d0_train,d1_train,d2_train,d3_train)
  d_test=rbind(d0_test,d1_test,d2_test,d3_test)
  
  
  d_train$y = as.factor(d_train$y)
  re_rf = randomForest(y~.,data = d_train)
  pred_rf = predict(re_rf,newdata = d_test,type="prob")
  tab_rf=table(predict(re_rf,d_test),d_test$y)
  errorate_pred_rf[i,]=(1-sum(diag(tab_rf))/sum(tab_rf))*100
}
min_pred_rf = which.min(errorate_pred_rf[which(rowSums(errorate_pred_rf)>0),])
pred_rf_v=c(errorate_pred_rf[min_pred_rf])

#6-5.KNN
errorate_pred_knn=matrix(rep(0,180),nrow=9)
for (i in 1:9)
{
  d0_train=d0[label0<=i,]
  d0_test=d0[label0>i,]
  d1_train=d1[label1<=i,]
  d1_test=d1[label1>i,]
  d2_train=d2[label2<=i,]
  d2_test=d2[label2>i,]
  d3_train=d3[label3<=i,]
  d3_test=d3[label3>i,]
  
  d_train=rbind(d0_train,d1_train,d2_train,d3_train)
  d_test=rbind(d0_test,d1_test,d2_test,d3_test)
  
  
  library(class)
  
  dctrain=dc[,-21]
  dc_train=d_train[,-21]
  dc_test=d_test[,-21]
  y_train=d_train[,21]
  y_test=d_test[,21]
  
  errorknn_train=c()
  for (j in 1:20)
  {
    knnvalue_train = knn(train=dc_train,test=dc_test,cl=y_train,k=j,prob=TRUE,use.all = TRUE)
    tab_knn=table(knnvalue_train,y_test)
    errorate_pred_knn[i,j]=(1-sum(diag(tab_knn))/sum(tab_knn))*100
  }
}

min_pred_knn = which.min(errorate_pred_knn)
pred_knn_v=c(errorate_pred_knn[min_pred_knn])

matrix= cbind(pred_log_v,pred_id3_v,pred_CART_v,pred_rf_v,pred_knn_v)
matrix

#6-6.SVM
re_svm=svm(y~.,data=d_train)
pred_re_svm=predict(re_svm,newdata=d_test,type="class")
tab_svm=table(pred_re_svm,d_test$y)
tab_svm

#7.做出好看的决策树
install.packages("rattle")
library(rattle)
library(rpart.plot)
library(rpart)
library(RColorBrewer)

fancyRpartPlot(re_CART_f)#已剪枝 cp=0.001
fancyRpartPlot(re_id3_f)

dt = read.csv("test.csv",header = TRUE)
dct=dt[,-1]
pred_log_test = predict(re_log,newdata = dct)
summary(pred_log_test)
pred_id3_test = predict(re_id3_f,newdata = dct,type = "class")
summary(pred_id3_test)
pred_CART_test = predict(re_CART_f,newdata = dct,type = "class")
summary(pred_CART_test)
pred_rf_test = predict(re_rf,newdata = dct)
summary(pred_rf_test)
pred_knn_test = knn(train=dctrain,test=dct,cl=d$y,k=which.min(apply(errorate_pred_knn,2,min)),prob=TRUE,use.all = TRUE)
summary(pred_knn_test)


#8.多重共线性
library(car)
lmd=lm(y~.-y,data=d)
vif(lmd)

lmd2=cor(d[,1:20])
kaapa(lmd2,exact=TRUE)

#9.预测
test=read.csv("test.csv",header = TRUE)

library(nnet)
re_log_test=multinom(y~.,data=dc[,-1])
pred_log_test=predict(re_log_test,newdata=test)
summary(pred_log_test)



