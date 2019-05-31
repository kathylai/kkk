d = read.csv("train1.csv",header = TRUE)
dc = d[,2:9]
sdc = scale(dc)#标准化
mean(sdc[,1])
cov_sdc = cov(sdc)
eigen(cov_sdc)#算出的八个特征向量各自的维度 求特征值 八个特征值 累计结果大于85%
princomp(dc)
library("stats",lib.loc = "C:/Program Files/Microsoft/R Open/R-3.4.0/library")
prcomp(dc)
prcomp(sdc)#说明该算法不支持标准化 需提前标准化
#factomineR 算法