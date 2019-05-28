x = 1:10000
plot(x,1-(1-1/x)^x)

store=rep(NA,10000)
for (i in 1:10000){
  store[i] = sum(sample(1:100, rep = TRUE) == 4)>0
}
x = 1:10000
plot(x,1-(1-1/x)^x)