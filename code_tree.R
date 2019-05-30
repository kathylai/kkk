d= read.csv("train.csv",header=TRUE)
dc= d[complete.cases(d),]
d0 = d[d$y==0,]
d1 = d[d$y==1,]
d2 = d[d$y==2,]
d3 = d[d$y==3,]

label0 = sample(c(1:10),dim(d0[1]),replace=TRUE)
label1 = sample(c(1:10),dim(d1[1]),replace=TRUE)
label2 = sample(c(1:10),dim(d2[1]),replace=TRUE)
label3 = sample(c(1:10),dim(d3[1]),replace=TRUE)

d0_train = d0[label0<=5,]
d0_test = d0[label0>5,]
d1_train = d1[label1<=5,]
d1_test = d1[label1>5,]
d2_train = d2[label2<=5,]
d2_test = d2[label2>5,]
d3_train = d3[label3<=5,]
d3_test = d3[label3>5,]
d_train = rbind(d0_train,d1_train,d2_train,d3_train)
d_test = rbind(d0_test,d1_test,d2_test,d3_test)

re_log = multinom(y~.-id,data=d_train)
pred_log = predict(re_log,newdata=d_test)
tab_log= table(d_test$y,pred_log)
library(nnet)
library(rpart)
re_id3_mis = rpart(y~.-id,data=d_train)
re_id3 = rpart(y~.-id,data=d_train,method="class",parms=list(split="information"),control=rpart.control(cp = 0.0001))
re_CART = rpart(y~.-id,data=d_train,method="class",parms=list(split="gini"),control=rpart.control(cp = 0.0001))
min = which.min(re_CART$cptable[,4])
re_CART_f = prune(re_CART,cp=re_CART$cptable[min,1])
pred_id3=predict(re_id3,newdata=d_test)
pred_CART=predict(re_CART_f,newdata=d_test,type="class")
table(d_test$y,pred_CART)

d_train$y= as.factor(d_train$y)
re_rf = randomForest(y~.-id,data=d_train,ntree=50)
pred_rf=predict(re_rf,newdata=d_test,type="prob")

pred <- prediction(pred_rf[,2], d_test$y)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
d_train$y[d_train$y>=1]=1
d_test$y[d_test$y>=1]=1

library(RWeka)