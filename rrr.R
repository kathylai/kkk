mse1=c()
for (i in 1:10) 
{
lr6 = glm(pm2.5~No,data = d3[d3$label!=i,])
pre3 = predict(lr6,newdata = d3[d3$label==i,])
error=pre3-d3[d3$label==i,]$pm2.5
mse1[i]=sum(error^2)/length(pre3)
}

mse2 = matrix(rep(0,150),nrow=10)
for (j in 1:15)
{
  for(i in 1:10)
  {
    lr7 = glm(pm2.5~poly(No,j),data=d3[d3$label!=i,])    (j为模型中变量“No”的指数)
    pre4 = predict(lr7,newdata= d3[d3$label==i,])
    error=pre4-d3[d3$label==i,]$pm2.5
    mse2[i,j]= sum(error^2)/length(pre4)
  }
}
mmse1=apply(mse2, 2, mean)
plot(mmse1)
lines(mmse1)

mse3=matrix(rep(0,1500),nrow = 10)
for (k in 1:10)
  {
  for (j in 1:15) 
    {
    for (i in 1:10) 
      {
      lr8=glm(pm2.5~poly(No,j)+poly(TEMP,k),data=d3[d3$label!=i,])
      pre5 = predict(lr8,newdata=d3[d3$label==i,])
      error=pre5-d3[d3$label==i,]$pm2.5
      mse3[i,15*(k-1)+j]=sum(error^2)/length(pre5)
    }
  }
}

power=which.min(mmse1)
lrf=glm(pm2.5~poly(No,power),data = train)
pref=predict(lrf,newdata=test)
errorf=pref-test$pm2.5
msef=sum(error^2)/length(pref)
msef

number=10000
bonus=20
pack=40
x=matrix(rep(0,pack*number),nrow = pack)
label=sample(c(1:40),40,replace = FALSE)
for (j in 1:number)
  {
  x[label=1,j]=max(0.01,round(runif(1,min = 0.01,max = bonus/pack*2),2)-0.01)
    for (i in 1:38) 
      {
        ulimit=(bonus-sum(x[,j]))/(pack-i)*2
        x[i+1,j]=max(0.01,round(runif(1,min=0.01,max=ulimit),2)-0.01)
        }
  x[pack,j]=bonus-sum(x[,j])
}

number=10000
bonus=20
pack=40
x=matrix(rep(0,pack*number),nrow = pack)
for (j in 1:number)
{
  x[1,j]=max(0.01,round(runif(1,min = 0.01,max = bonus/pack*2),2)-0.01)
  for (i in 1:38) 
  {
    ulimit=(bonus-sum(x[,j]))/(pack-i)*2
    x[i+1,j]=max(0.01,round(runif(1,min=0.01,max=ulimit),2)-0.01)
  }
  x[pack,j]=bonus-sum(x[,j])
  x[,j]=sample(x[,j],40,replace=FALSE)
}
