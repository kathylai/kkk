d = read.csv("train.csv",header = TRUE)
dc = d[,2:9]
sdc = scale(dc)#��׼��
mean(sdc[,1])
cov_sdc = cov(sdc)
eigen(cov_sdc)
princomp(dc)
library("stats",lib.loc = "C:/Program Files/Microsoft/R Open/R-3.4.0/library")
prcomp(dc)
prcomp(sdc)#˵�����㷨��֧�ֱ�׼�� ����ǰ��׼��
#factomineR �㷨