CH2
#1.基本命令
ls()   查看所有对象列表
rm()  #消除对象 单个+所有 rm(list=ls())
rnorm() #产生一个随机正太变量的向量    默认均值0 标准差1
cor()  计算相关系数矩阵-所有预测变量两两之间相关系数矩阵  
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=0.1)
cor(x,y)  &  cor(smarket[,-9])
set.seed() #完全相同的一组随机数 数字为编号 生成数列setseed储存 需要再调用
sd() sqrt(var(y))  标准差

seq()创建序列 
seq(0,2,length=20)  -0~2之间等距的序列 
seq(1:10) =seq(1,10)   1~10 距离为1


#2.数据导入
read.table()   导入
write.table()    导出
dim()   数据行列信息
na.omit() /  A[complete.cases(A),]   剔除缺数据的行

#3.图形
contour()  等高线图
image()  热地图
persp()    三维图
plot()     散点图
hist()    直方图
pairs()   对任何指定数据集中每一对变量的散点图矩阵



CH3
library(MASS)

#1.简单线性回归
lm(y~x,data = ) 拟合简单的线性回归
attach()  绑定数据集
confint(lm)   得到系数估计值的置信区间
predict(lm,data,interval="confidence")       #interval 置信区间
plot(lm)     直接生成残差诊断图-par(mfrom=c(2,2))

#2.多元线性回归
lm()  #big data
library(car)
vif()      方差膨胀因子
#3.交叉性
lm(y~x1*x2,data=)  #x1,x2,x1*x2三项的回归
#4.非线性
anova(lm1,lm2)  量化两种不同的拟合



CH4
#1.逻辑回归
glm(y~x,data=,family = binomial)  #bin默认逻辑 不设置family则同lm   
predict(glm,data=,type="response")
contrasts()  创建哑变量

glm.fit=glm(purchase~.,data=caravan,family = binomial,subset = -test)   
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>0.5]="Yes"    #或者直接分
table(glm.pred,test.y)
mean(glm.pred==derection.2005)

#2.Liner Discriminant Analysis 线性判别-等方差
library(MASS)
lda.fit=lda(y~.,data=,subset=train)    #subset -对行的约束条件
predict(lda.fit,smarket.2005)
table&mean

#4.二次判别-异方差
library(MASS)
qda.fit=qda(y~.,data=,subset=train)    
predict(qda.fit,smarket.2005)

#5.KNN
library(class)
set.seed(1)
knn.pred=knn(train,test,Y,k=) #训练集+测试集+训练观测类标签向量+k
#table&mean(test.y!==knn.pred)



CH5
set.seed #随机数生成器设定种子
sample(392.196)   #392个数据中自动选择196个

#1.留一交叉验证LOOCV
#广义线性模型的交叉验证 cv.glm(data, glmfit, cost, K)
cv.error=rep(0,5)    #存在第i个元素
for (i in 1:5){
  glm.fit=glm(mpg~poly(house,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]   #delta:原始交叉验证+调整后交叉验证-一般相等
}

#2.k折交叉验证  k-fold CV
set.seed(17)
cv.error.10=rep(0,10)    
for (i in 1:10){
  glm.fit=glm(mpg~poly(house,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]   #delta:原始交叉验证+调整后交叉验证-一般相等
}

#3.自助法   bootstrap   -几乎所有
#boot(data,statistic,R)    R为bootstrap的重复次数
#statistic的参数必须包含计算的数据集data和测试数据的索引index-建立一个function 再往fn里放东西
boot.fn=function(data,index)
  coefficients(lm(mpg~horse+I(horse)^2),data=data,subset=index)
set.seed(1)
boot(Auto,boot.fn,1000)
#____
alpha.fn(Portfolio,sample(100,10,replace=T))  #1-100有放回的抽取10个数字


CH8
library(tree)
data.frame() #转化数据框 合并数据[列名-变量名]

#1.构建分类树-定性变量属于哪类
tree.carseats=tree(High~.,Carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)  #pretty=0输出所有定性预测变量的类别名-而不是首字母
predict(tree.carseats,Carseats.test,type="class")
cv.tree(tree.carseats,FUN=prune.misclass)  #交叉验证 用分类错误率


#2.构建回归树
https://blog.csdn.net/weixin_43216017/article/details/87739323
#自上而下(树顶端开始)的贪婪法(每一步最优)-递归二叉分裂(变量x 分割点s RSSmin)
         #直到分裂使残差平方和RSS减小量超过某阈值时才分裂树节点
#剪枝-过拟合: 选出测试及预测误差min的字数
#1) classification error rate 分类错误率-不纯度
#2)GINI INDEX 基尼系数-纯度(越小同类)
#3)cross-entropy 互熵-类gini
library(rpart)
rpart
rpart(formula,data,weights,subsets,na.action=na.rpart,method,parms,control) 
#parms	只用于分类树，parms=list(split,prior,loss)，其中split的选项默认"gini"（对应CART），和"information"（对应ID3算法）

#1) ID3 Information Gain 选择信息增益最大的方向进行分支标准
re_id3=rpart(y~.-id,data=d_train,method="class",parms = list(split="information"))
pred_id3=predict(re_id3,newdata = d_test,type="class")
pred_id3
table(d_test$y,pred_id3)

#2) C4.5 -Gain Ratio/GAIN entropy  信息熵越大说明事件的无序程度越高特征越重要

#3)cart  Gini Index越小越纯 最小的切分点最为最优切分点
re_CART=rpart(y~.-id,data=d_train,method="class",parms = list(split="gini"))#同原id3 默认基尼
pred_CART=predict(re_CART,newdata = d_test,type="class")
table(d_test$y,pred_CART)

#4) 剪枝     
(1)#——通过cp控制剪枝范围  默认0.01 选择交叉误差最小的数(xerror第四列)
#cp是参数复杂度complexity parameter-作为控制树规模的惩罚因子，cp越大树分裂规模（nsplit）越小
#输出参数（rel error）指示了当前分类模型树与空树之间的平均偏差比值。xerror为交叉验证误差，xstd为交叉验证误差的标准差。
#可以看到，当nsplit为3的时候，即有四个叶子结点的树，要比nsplit为4，即五个叶子结点的树的交叉误差要小。而决策树剪枝的目的就是为了得到更小交叉误差（xerror）的树。

re_id3$cptable
#cp=0.01 但还是不够___还要在小
re_CART=rpart(y~.-id,data=d_train,method="class",parms = list(split="gini"),control=rpart.control(cp=0.05))
re_CART
re_CART$cptable
#cp=0.05 数字太大 只有一个节点

#0.001
re_CART=rpart(y~.-id,data=d_train,method="class",parms = list(split="gini"),control=rpart.control(cp=0.001))
re_CART
re_CART$cptable
plot(re_CART)
text(re_CART)     #误差最小0.001~0.796
pred_CART=predict(re_CART,newdata = d_test,type="class")
table(d_test$y,pred_CART)

#cp=0.0001
re_CART=rpart(y~.-id,data=d_train,method="class",parms = list(split="gini"),control=rpart.control(cp=0.0001))
re_CART$cptable

#选择最小的
min=which.min(re_CART$cptable[,4])

(2)剪枝   # prune(tree,cp,…)
re_CART_f=prune(re_CART,cp=re_CART$cptable[min,1])   #自动选择xerror最小时候对应的cp值来剪枝
pred_CART=predict(re_CART,newdata = d_test,type="class")
table(d_test$y,pred_CART)

(3)画图
rpart.plot(tree,type,fallen.leaves=T,branch,…)

#3.袋装法&随机森林 bagging &random forest
#bagging 不断放回地对训练样本进行再抽样 对每个自助样本都建立一棵回归树 对于每一个观测 每棵树给一个预测 最后将其平均
#rf 仅对样本进行抽样，还对变量进行抽样
install.packages("randomForest")
library(randomForest)
d_train$y=as.factor(d_train$y)
re_rf=randomForest(y~.-id,data=d_train,ntree=5)

for(i in 1:(n-1)){
  set.seed(1234)
  rf_train<-randomForest(as.factor(train_data$IS_LIUSHI)~.,data=train_data,mtry=i,ntree=1000)
  rate[i]<-mean(rf_train$err.rate)   #计算基于OOB袋外数据的模型误判率均值
  print(rf_train)    
}
#mtry考虑变量 装袋法-全体变量 回归p/3  分类-根号p

#prob 概率
library(prediction)
install.packages("ROCR")
library(ROCR)
pred<- prediction(pred_rf[,2],d_test$y)
perf<-performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

#4.提升法  booting
#提高弱分类算法准确度  采取少数服从多数的原理  eg加权重使得类别相同
gbmlibrary('gbm')
adaboost.gbm=gbm(y1~.,data=train.data, dist="adaboost",n.tree=50,interaction.depth=10)
#回归 distribution="gaussian", 二分类="bernoulli"  depth-树的深度




CH9  SVM
library(e1071)
#1.SVM分类器
dat=data.frame(x=x,y=as.factor(y))   #把响应变量编码为因子变量数据框
avmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)  
#kerner-拟合svm分类器:线性-分类  cost-设置观测穿过间隔的成本-越小间隔越宽 更多支持向量落在间隔上/穿过间隔
tune(svm,y~.,data=dat,kernel="linear",range=list(cost=c(0.001,0.01,0.1,1,5,10)))
#交叉验证-默认十折


#2.SVM
#kernel="polynomial"-拟合多项式核函数 ~degree阶数  ="radial"拟合径向基核函数 ~gamma

#3.ROC
library(ROCR)
rocplot=function(pred,truth,...){
  predob=predict(pred,truth)
  pref=performance(predob,"trp","fpr")
  plot(perf)
}
rocplot(fitted,dat[train,"y"],add=T,col="red")

#4.多分类SVM
#响应因子水平数超过2，SVM一类对一类 先生成第三类观测
sumfit=svm(y~.,data=dat,kernel="radial",cost=10,gamma=1) 





CH10 
apply(USArrests,2,var) #对数据集每一行/列使用用一函数  行1列2 
cumsum()   #计算数值向量中元素的累计和
#1. PCA
##PCA降维 维度比个数多    更集中化 肯定会漏信息
sdc=scale(dc)    #标准化 ——上网找 均值0 方差1
mean(sdc[,1])    #我们专业：-17次方可以算零
cov_sdc= cov(sdc)    #协方差矩阵
eigen(cov_sdc)      #特征值

princomp(dc)   #变量之间的相关关系 cor来决定是通过correlation还是covariance来计算 
library(stats)
prcomp(data,scale=TRUE) #样本之间的相关关系  默认中心化处理 scale标准化
#$center/$scale/$sdev 标准化后变量的均值/标准差/每个PCA标准差    $rotation 对应pca载荷向量-旋转矩阵
dim(pr.out$x) #-50 4 x的列就是PcA得分向量-x矩阵第k列=第k个PCA得分向量

#2.cluster
(1) K-mean
km.out=kmeans(x,3,nstart=20)  #k=3 nstart-试验初始类的分配情况
(2) 系统聚类法


#3.神经网络
library(nnet)
re_log=multinom(y~.-id,data=d_train)
#不断迭代 
#默认跑100次 100次之后没有最优结果就不跑了—会死循环
#次数可以从数据包内部修改——eg.10 200



