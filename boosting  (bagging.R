n = 100
alpha = c()
for(i in 1:1000)
{
  mu1= c(0,0)
sigma1=matrix(c(1,0.5,0.5,1.25),nrow = 2)
#定义参数
rand1 = mvrnorm(n=100,mu = mu1, Sigma=sigma1)
X = rand1[,1]
Y= rand1[,2]
alpha[i] = (var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
}
mean(alpha)
mean(sqrt(alpha))

label = c(1:100)
rand=cbind(rand1,label)
lab = sample(c(1:100),1,replace = TRUE)
ran=rand1[label==lab,]
for(j in 1:99)
{
lab=sample(rand1,1,replace = TRUE)
ran2=rand1[label==lab,]
ran = rbind(ran,ran2)
}
X = rand1[,1]
Y= rand1[,2]
alpha[j] = (var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))

rand1[sample(c(1:100),100,replace=TRUE),]
sample(c(1:100),100,replace=TRUE)
#蒙特卡洛仿真
mean(alpha)
var(alpha)
sd(alpha)
