Power = function(){
  print(2^3)
}
Power()
Power2 = function(x,a){
  print(x^a)
}
Power2(3,8)
Power3 = function(x,a){
  result = x^a
return(result)
}#存储了特征值
x = c(1:10)
y = Power3(x,2)#一定写Power3函数 看实现的过程
plot(x,y)#可以取对数降skill
PlotPower = function(x,a){
  y = Power3(x,a)
  plot(x,y)
}
PlotPower(1:30,3)
#自定义画图？plot 可视化

set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x-2*x^2+rnorm(100)
plot(x,y)

d = cbind(x,y)
error = c()
d = as.data.frame(d)
for (i in 1:100) {
m1 = glm(y~x,data = d[i,])
pred_m1 = predict(m1, newdata = d[i,])
error[i] = d[i,2]-pred_m1
}
sum(error^2)


m1 = glm(y~poly(x,1),data = d)
m1r = cv.glm(data=d,glmfit = m1, K = 100)
m1r$delta
m2 = glm(y~poly(x,2),data = d)
m2r = cv.glm(data=d,glmfit=m2,K=100)
m2r$delta
m3 = glm(y~poly(x,3),data = d)
m3r = cv.glm(data=d,glmfit=m3,K=100)
m3r$delta
m4 = glm(y~poly(x,4),data = d)
m4r = cv.glm(data=d,glmfit=m4,K=100)
m4r$delta
