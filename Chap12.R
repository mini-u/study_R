#12. 군집화와 분류

#K-means 클러스터링
ds <- iris[, 1:4]

fit <- kmeans(x=ds, centers=3)
fit
fit$cluster
fit$centers #각 군집의 중심점

library(cluster)
clusplot(ds, fit$cluster, color=T, shade=T, labels = 1, lines=1)
subset(ds, fit$cluster==2)

std <- function(X) {
  return( (X-min(X)) / (max(X) - min(X)))
}

ds <- apply(iris[,1:4], 2, std)

fit <- kmeans(x=ds, centers=3)
fit


# k nn 알고리즘
library(class)

tr.inx <- c(1:25, 51:75, 101:125)
ds.tr <- iris[tr.inx, 1:4]
ds.ts <- iris[-tr.inx, 1:4]
cl.tr <- factor(iris[tr.inx, 5])
cl.ts <- factor(iris[-tr.inx, 5])

pred <- knn(ds.tr, ds.ts, cl.tr, k=3, prob=T)
pred

acc <- mean(pred ==cl.ts)
acc


# k fold 교차검증
library(cvTools)

k=10
folds <- cvFolds(nrow(iris), K=k)

acc <- c()
for (i in 1:k) {
  ts.idx <- folds$which ==i
  ds.tr <- iris[-ts.idx, 1:4]
  ds.ts <- iris[ts.idx, 1:4]
  cl.tr <- factor(iris[-ts.idx, 5])
  cl.ts <- factor(iris[ts.idx, 5])
  
  pred <- knn(ds.tr, ds.ts, cl.tr, k=5)
  acc[i] <- mean(pred==cl.ts)
}

acc
mean(acc)
acc


# 실전 분석
library(carData)
str(UN)
temp <- UN[,3:7]
temp <- temp[complete.cases(temp), ]

temp <- scale(temp)

fit <- kmeans(temp, center=3)
grp <- fit$cluster
grp

agg <- aggregate(UN[complete.cases(UN), c('ppgdp', 'infantMortality')],
                 by=list(grp=grp), mean)
agg$grp <- factor(agg$grp)
agg

library(ggplot2)
ggplot(agg, aes(x=grp, y=ppgdp))+
  geom_bar(stat='identity', width=0.7, fill='steelblue')+
  ggtitle('그룹별 1인당 국내 총 생산')
ggplot(agg, aes(x=grp, y=infantMortality))+
  geom_bar(stat='identity', width=0.7, fill='orange')+
  ggtitle('그룹별 영아 사망률')


