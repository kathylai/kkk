d = read.csv("train.csv",header = TRUE)
View(d)
#清洗数据
dc= d[complete.cases(d),]#无空值，需要这一句检查的过程
hist(d$y)#分布#优先分组y防止抽样不均
#分类
d0 = d[d$y == 0,]
d1 = d[d$y == 1,]
d2 = d[d$y == 2,]
d3 = d[d$y == 3,]
#贴标签
label0 = sample(c(1:10),dim(d0[1]),replace=TRUE)
label1 = sample(c(1:10),dim(d1[1]),replace=TRUE)
label2 = sample(c(1:10),dim(d2[1]),replace=TRUE)
label3 = sample(c(1:10),dim(d3[1]),replace=TRUE)
tab = list()
#分训练集和测试集
d0_train = d0[label0<=9,]
d0_test = d0[label0>9,]
d1_train = d1[label1<=9,]
d1_test = d1[label1>9,]
d2_train = d2[label2<=9,]
d2_test = d2[label2>9,]
d3_train = d3[label3<=9,]
d3_test = d3[label3>9,]
d_train = rbind(d0_train,d1_train,d2_train,d3_train)
d_test = rbind(d0_test,d1_test,d2_test,d3_test)
#数据分类比例需标明
re_glm = glm(y~.-id,data = d_train,family = binomial)
#binomial代表二分类 glm不适应与此分析，-id表示除去id
#multinomial
library(nnet)
re_log = multinom(y~.,data = d_train)
#运行到一百也没有达到平衡于是就在一百停止
pred_log = predict(re_log,newdata = d_test)
summary(pred_log)
tab_log = table(d_test$y,pred_log)
tab_log#列代表真正的数据（2092+11406+……）

library(rpart)#ID3 decision tree
re_id3_mistake = rpart(y~.-id,data = d_train)#将数据默认为连续变量
re_id3 = rpart(y~.-id,data = d_train,method = "class",parms = list(split="information"))#将数据当成分类变量
plot(re_id3)
text(re_id3)
re_id3#末端是整数是需要的
re_CART = rpart(y~.,data = d_train,method = "class",parms = list(split="gini"))
pred_id3 = predict(re_id3,newdata = d_test,type = "class")
table(d_test$y,pred_id3)
pred_CART = predict(re_CART,newdata = d_test,type = "class")
table(d_test$y,pred_CART)

re_id3$cptable#说明cp（α）越大越难以分解，选择交叉验证误差最低的cp值，默认最小值0.01
re_CART = rpart(y~.-id,data = d_train,method = "class",parms = list(split="gini"),control = rpart.control(cp=0.0001))
re_CART$cptable
table(d_test$y,pred_CART)
min = which.min(re_CART$cptable[,4])
min = which.min(re_id3$cptable[,4])
re_CART_f = prune(re_CART,cp=re_CART$cptable[min,1])
re_id3_f = prune(re_id3,cp=re_id3$cptable[min,1])
pred_CART = predict(re_CART_f,newdata = d_test,type = "class")
pred_id3 = predict(re_id3_f,newdata = d_test,type = "class")
table(d_test$y,pred_CART)
plot(re_CART_f)
text(re_CART_f)
install.packages("randomForest")
library("randomForest")
d_train$y = as.factor(d_train$y)
re_rf = randomForest(y~.,data = d_train)
pred_rf = predict(re_rf,newdata = d_test,type="prob")
test_matrix=table(predict(re_rf,d_test),d_test$y)
1-sum(diag(test_matrix))/sum(test_matrix)

install.packages("prediction")
library("prediction")
install.packages("gplots")
library("gplots")
install.packages("ROCR")
library("ROCR")
pred<-prediction(pred_rf[,2],d_test$y)
perf<-performance(pred,"tpr","fpr")
#library
plot(perf,coloze=TRUE)

d_train$y[d_train$y>=1]=1
d_test$y[d_test$y>=1]=1
library(RWeka)#C4.5需安装java
install.packages("rattle")
library(rattle)
library(rpart.plot)
library(rpart)
library(RColorBrewer)

fancyRpartPlot(re_CART_f)

cart <-table(d_test$y,pred_CART)
1-sum(diag(cart))/sum(cart)
#[1] 0.1505922
id3 = table(d_test$y,pred_id3)
1-sum(diag(id3))/sum(id3)
#[1] 0.1404399
1-sum(diag(tab_log))/sum(tab_log)
#[1] 0.03553299
dt = read.csv("test.csv",header = TRUE)
pred_log = predict(re_log,newdata = dt)
summary(pred_log)
#0   1   2   3 
#252 232 257 259 
summary(pred_CART_t)
#0   1   2   3 
#233 246 242 279 
summary(pred_id3_t)
#0   1   2   3 
#245 250 248 257 