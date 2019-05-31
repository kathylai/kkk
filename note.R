CH2
#1.��������
ls()   �鿴���ж����б�
rm()  #�������� ����+���� rm(list=ls())
rnorm() #����һ�������̫����������    Ĭ�Ͼ�ֵ0 ��׼��1
cor()  �������ϵ������-����Ԥ���������֮�����ϵ������  
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=0.1)
cor(x,y)  &  cor(smarket[,-9])
set.seed() #��ȫ��ͬ��һ������� ����Ϊ��� ��������setseed���� ��Ҫ�ٵ���
sd() sqrt(var(y))  ��׼��

seq()�������� 
seq(0,2,length=20)  -0~2֮��Ⱦ������ 
seq(1:10) =seq(1,10)   1~10 ����Ϊ1


#2.���ݵ���
read.table()   ����
write.table()    ����
dim()   ����������Ϣ
na.omit() /  A[complete.cases(A),]   �޳�ȱ���ݵ���

#3.ͼ��
contour()  �ȸ���ͼ
image()  �ȵ�ͼ
persp()    ��άͼ
plot()     ɢ��ͼ
hist()    ֱ��ͼ
pairs()   ���κ�ָ�����ݼ���ÿһ�Ա�����ɢ��ͼ����



CH3
library(MASS)

#1.�����Իع�
lm(y~x,data = ) ��ϼ򵥵����Իع�
attach()  �����ݼ�
confint(lm)   �õ�ϵ������ֵ����������
predict(lm,data,interval="confidence")       #interval ��������
plot(lm)     ֱ�����ɲв����ͼ-par(mfrom=c(2,2))

#2.��Ԫ���Իع�
lm()  #big data
library(car)
vif()      ������������
#3.������
lm(y~x1*x2,data=)  #x1,x2,x1*x2����Ļع�
#4.������
anova(lm1,lm2)  �������ֲ�ͬ�����



CH4
#1.�߼��ع�
glm(y~x,data=,family = binomial)  #binĬ���߼� ������family��ͬlm   
predict(glm,data=,type="response")
contrasts()  �����Ʊ���

glm.fit=glm(purchase~.,data=caravan,family = binomial,subset = -test)   
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>0.5]="Yes"    #����ֱ�ӷ�
table(glm.pred,test.y)
mean(glm.pred==derection.2005)

#2.Liner Discriminant Analysis �����б�-�ȷ���
library(MASS)
lda.fit=lda(y~.,data=,subset=train)    #subset -���е�Լ������
predict(lda.fit,smarket.2005)
table&mean

#4.�����б�-�췽��
library(MASS)
qda.fit=qda(y~.,data=,subset=train)    
predict(qda.fit,smarket.2005)

#5.KNN
library(class)
set.seed(1)
knn.pred=knn(train,test,Y,k=) #ѵ����+���Լ�+ѵ���۲����ǩ����+k
#table&mean(test.y!==knn.pred)



CH5
set.seed #������������趨����
sample(392.196)   #392���������Զ�ѡ��196��

#1.��һ������֤LOOCV
#��������ģ�͵Ľ�����֤ cv.glm(data, glmfit, cost, K)
cv.error=rep(0,5)    #���ڵ�i��Ԫ��
for (i in 1:5){
  glm.fit=glm(mpg~poly(house,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]   #delta:ԭʼ������֤+�����󽻲���֤-һ�����
}

#2.k�۽�����֤  k-fold CV
set.seed(17)
cv.error.10=rep(0,10)    
for (i in 1:10){
  glm.fit=glm(mpg~poly(house,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]   #delta:ԭʼ������֤+�����󽻲���֤-һ�����
}

#3.������   bootstrap   -��������
#boot(data,statistic,R)    RΪbootstrap���ظ�����
#statistic�Ĳ������������������ݼ�data�Ͳ������ݵ�����index-����һ��function ����fn��Ŷ���
boot.fn=function(data,index)
  coefficients(lm(mpg~horse+I(horse)^2),data=data,subset=index)
set.seed(1)
boot(Auto,boot.fn,1000)
#____
alpha.fn(Portfolio,sample(100,10,replace=T))  #1-100�зŻصĳ�ȡ10������


CH8
library(tree)
data.frame() #ת�����ݿ� �ϲ�����[����-������]

#1.����������-���Ա�����������
tree.carseats=tree(High~.,Carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)  #pretty=0������ж���Ԥ������������-����������ĸ
predict(tree.carseats,Carseats.test,type="class")
cv.tree(tree.carseats,FUN=prune.misclass)  #������֤ �÷��������


#2.�����ع���
https://blog.csdn.net/weixin_43216017/article/details/87739323
#���϶���(�����˿�ʼ)��̰����(ÿһ������)-�ݹ�������(����x �ָ��s RSSmin)
         #ֱ������ʹ�в�ƽ����RSS��С������ĳ��ֵʱ�ŷ������ڵ�
#��֦-�����: ѡ�����Լ�Ԥ�����min������
#1) classification error rate ���������-������
#2)GINI INDEX ����ϵ��-����(ԽСͬ��)
#3)cross-entropy ����-��gini
library(rpart)
rpart
rpart(formula,data,weights,subsets,na.action=na.rpart,method,parms,control) 
#parms	ֻ���ڷ�������parms=list(split,prior,loss)������split��ѡ��Ĭ��"gini"����ӦCART������"information"����ӦID3�㷨��

#1) ID3 Information Gain ѡ����Ϣ�������ķ�����з�֧��׼
re_id3=rpart(y~.-id,data=d_train,method="class",parms = list(split="information"))
pred_id3=predict(re_id3,newdata = d_test,type="class")
pred_id3
table(d_test$y,pred_id3)

#2) C4.5 -Gain Ratio/GAIN entropy  ��Ϣ��Խ��˵���¼�������̶�Խ������Խ��Ҫ

#3)cart  Gini IndexԽСԽ�� ��С���зֵ���Ϊ�����зֵ�
re_CART=rpart(y~.-id,data=d_train,method="class",parms = list(split="gini"))#ͬԭid3 Ĭ�ϻ���
pred_CART=predict(re_CART,newdata = d_test,type="class")
table(d_test$y,pred_CART)

#4) ��֦     
(1)#����ͨ��cp���Ƽ�֦��Χ  Ĭ��0.01 ѡ�񽻲������С����(xerror������)
#cp�ǲ������Ӷ�complexity parameter-��Ϊ��������ģ�ĳͷ����ӣ�cpԽ�������ѹ�ģ��nsplit��ԽС
#���������rel error��ָʾ�˵�ǰ����ģ���������֮���ƽ��ƫ���ֵ��xerrorΪ������֤��xstdΪ������֤���ı�׼�
#���Կ�������nsplitΪ3��ʱ�򣬼����ĸ�Ҷ�ӽ�������Ҫ��nsplitΪ4�������Ҷ�ӽ������Ľ������ҪС������������֦��Ŀ�ľ���Ϊ�˵õ���С������xerror��������

re_id3$cptable
#cp=0.01 �����ǲ���___��Ҫ��С
re_CART=rpart(y~.-id,data=d_train,method="class",parms = list(split="gini"),control=rpart.control(cp=0.05))
re_CART
re_CART$cptable
#cp=0.05 ����̫�� ֻ��һ���ڵ�

#0.001
re_CART=rpart(y~.-id,data=d_train,method="class",parms = list(split="gini"),control=rpart.control(cp=0.001))
re_CART
re_CART$cptable
plot(re_CART)
text(re_CART)     #�����С0.001~0.796
pred_CART=predict(re_CART,newdata = d_test,type="class")
table(d_test$y,pred_CART)

#cp=0.0001
re_CART=rpart(y~.-id,data=d_train,method="class",parms = list(split="gini"),control=rpart.control(cp=0.0001))
re_CART$cptable

#ѡ����С��
min=which.min(re_CART$cptable[,4])

(2)��֦   # prune(tree,cp,��)
re_CART_f=prune(re_CART,cp=re_CART$cptable[min,1])   #�Զ�ѡ��xerror��Сʱ���Ӧ��cpֵ����֦
pred_CART=predict(re_CART,newdata = d_test,type="class")
table(d_test$y,pred_CART)

(3)��ͼ
rpart.plot(tree,type,fallen.leaves=T,branch,��)

#3.��װ��&���ɭ�� bagging &random forest
#bagging ���ϷŻصض�ѵ�����������ٳ��� ��ÿ����������������һ�ûع��� ����ÿһ���۲� ÿ������һ��Ԥ�� �����ƽ��
#rf �����������г��������Ա������г���
install.packages("randomForest")
library(randomForest)
d_train$y=as.factor(d_train$y)
re_rf=randomForest(y~.-id,data=d_train,ntree=5)

for(i in 1:(n-1)){
  set.seed(1234)
  rf_train<-randomForest(as.factor(train_data$IS_LIUSHI)~.,data=train_data,mtry=i,ntree=1000)
  rate[i]<-mean(rf_train$err.rate)   #�������OOB�������ݵ�ģ�������ʾ�ֵ
  print(rf_train)    
}
#mtry���Ǳ��� װ����-ȫ����� �ع�p/3  ����-����p

#prob ����
library(prediction)
install.packages("ROCR")
library(ROCR)
pred<- prediction(pred_rf[,2],d_test$y)
perf<-performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

#4.������  booting
#����������㷨׼ȷ��  ��ȡ�������Ӷ�����ԭ��  eg��Ȩ��ʹ�������ͬ
gbmlibrary('gbm')
adaboost.gbm=gbm(y1~.,data=train.data, dist="adaboost",n.tree=50,interaction.depth=10)
#�ع� distribution="gaussian", ������="bernoulli"  depth-�������




CH9  SVM
library(e1071)
#1.SVM������
dat=data.frame(x=x,y=as.factor(y))   #����Ӧ��������Ϊ���ӱ������ݿ�
avmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)  
#kerner-���svm������:����-����  cost-���ù۲⴩������ĳɱ�-ԽС���Խ�� ����֧���������ڼ����/�������
tune(svm,y~.,data=dat,kernel="linear",range=list(cost=c(0.001,0.01,0.1,1,5,10)))
#������֤-Ĭ��ʮ��


#2.SVM
#kernel="polynomial"-��϶���ʽ�˺��� ~degree����  ="radial"��Ͼ�����˺��� ~gamma

#3.ROC
library(ROCR)
rocplot=function(pred,truth,...){
  predob=predict(pred,truth)
  pref=performance(predob,"trp","fpr")
  plot(perf)
}
rocplot(fitted,dat[train,"y"],add=T,col="red")

#4.�����SVM
#��Ӧ����ˮƽ������2��SVMһ���һ�� �����ɵ�����۲�
sumfit=svm(y~.,data=dat,kernel="radial",cost=10,gamma=1) 





CH10 
apply(USArrests,2,var) #�����ݼ�ÿһ��/��ʹ����һ����  ��1��2 
cumsum()   #������ֵ������Ԫ�ص��ۼƺ�
#1. PCA
##PCA��ά ά�ȱȸ�����    �����л� �϶���©��Ϣ
sdc=scale(dc)    #��׼�� ���������� ��ֵ0 ����1
mean(sdc[,1])    #����רҵ��-17�η���������
cov_sdc= cov(sdc)    #Э�������
eigen(cov_sdc)      #����ֵ

princomp(dc)   #����֮�����ع�ϵ cor��������ͨ��correlation����covariance������ 
library(stats)
prcomp(data,scale=TRUE) #����֮�����ع�ϵ  Ĭ�����Ļ����� scale��׼��
#$center/$scale/$sdev ��׼��������ľ�ֵ/��׼��/ÿ��PCA��׼��    $rotation ��Ӧpca�غ�����-��ת����
dim(pr.out$x) #-50 4 x���о���PcA�÷�����-x�����k��=��k��PCA�÷�����

#2.cluster
(1) K-mean
km.out=kmeans(x,3,nstart=20)  #k=3 nstart-�����ʼ��ķ������
(2) ϵͳ���෨


#3.������
library(nnet)
re_log=multinom(y~.-id,data=d_train)
#���ϵ��� 
#Ĭ����100�� 100��֮��û�����Ž���Ͳ����ˡ�����ѭ��
#�������Դ����ݰ��ڲ��޸ġ���eg.10 200


