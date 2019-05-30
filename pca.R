d = read.csv("train.csv",header = TRUE)
dc = d[,2:9]
sdc = scale(dc)#标准化
mean(sdc[,1])
cov_sdc = cov(sdc)
eigen(cov_sdc)
princomp(dc)
library("stats",lib.loc = "C:/Program Files/Microsoft/R Open/R-3.4.0/library")
prcomp(dc)
prcomp(sdc)#说明该算法不支持标准化 需提前标准化
#factomineR 算法