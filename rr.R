newd = d[!is.na(d$pm2.5),]
train = newd[newd$year<=2013,]
test =newd[newd$year==2014,]
label = sample(c(1:10),dim(train)[1],replace = TRUE)
newtrain = cbind(train,label)

mse = matrix(rep(0,200),nrow=10)
for(j in 1:20)
{
  for(i in 1:10)
  {
    lr = glm(pm2.5~poly(TEMP,j),data=newtrain[newtrain$label!=i,])
    pre = predict(lr,newdata=newtrain[newtrain$label==i,])
    mse[i,j]=sum((pre-newtrain[newtrain$label==i,]$pm2.5)^2)/length(pre)
  }
}

mmse = apply(mse,2,mean)
which.min(mmse)
lrf = glm(pm2.5~poly(TEMP,19),data=newtrain)
pref = predict(lrf,newdata=test)
summary(lrf)

bonus = 20
pack = 40
x[1]=max(0.01,round(runif(1,min = 0.01,max = 20/40*2),2)-0.01)
x[2]=max(0.01,round(runif(1,min = 0.01,max = (20-x[1])/(40-1)*2),2)-0.01)

x= rep(0,40)
bonus = 20
pack = 40
x[1]=max(0.01,round(runif(1,min = 0.01,max = bonus/pack*2),2)-0.01) 
for(i in 1:38)
{
ulimit=(bonus-sum(x))/(pack-i)*2
x[i+1]=max(0.01,round(runif(1,min = 0.01,max = ulimit),2)-0.01)
}
x[pack]=bonus-sum(x)

x= matrix(rep(0,pack*number),nrow=pack)
number = 10000
bonus = 20
pack = 40
for(j in 1:number)
{
x[1,j]=max(0.01,round(runif(1,min = 0.01,max = bonus/pack*2),2)-0.01) 
for(i in 1:38)
{
  ulimit=(bonus-sum(x[,j]))/(pack-i)*2
  x[i+1,j]=max(0.01,round(runif(1,min = 0.01,max = ulimit),2)-0.01)
}
x[pack,j]=bonus-sum(x[,j])
}

re=apply(x,1,mean)
plot(re)
lines(re)