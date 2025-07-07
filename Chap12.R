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


#연습문제
#01
library(cluster)

head(Seatbelts)
ds <- Seatbelts[,-8]
std <- function(X){
  return((X-min(X)) / (max(X)-min(X)))
}
std_ds <- apply(ds, 2, std)
std_ds
 
fit <- kmeans(x=std_ds, centers=2)
clusplot(ds, fit$cluster, color=T, shade=T, labels = 1, lines=0)


#02
library(mlbench)
ds <- data.frame(Sonar)
ds <- ds[, -61]
head(ds)

fit <- kmeans(x=ds, centers=2)
clusplot(ds, fit$cluster, color=T, shade=T, labels=1)


#03
head(swiss)
fit <- kmeans(x=swiss, centers=3)
clusplot(swiss, fit$cluster, color=T, shade = T, lable=1)

std_ds <- apply(swiss, 2, std)
fit <- kmeans(x=std_ds, centers = 3)
clusplot(std_ds, fit$cluster, color=T, shade=T, labels=1)


#04
head(rock)
std_rock <- apply(rock, 2, std)
fit <- kmeans(x=std_rock, centers=3)
clusplot(std_rock, fit$cluster, color=T, shade=T, labels=1)


#05
library(mlbench)
library(class)
ds <- data.frame(Soybean)
str(ds)

#결측값 제거
sum(is.na(ds))
ds_clean <- ds[complete.cases(ds),]
str(ds_clean)

#훈련, 테스트데이터 분리
tr.idx <- seq(2, nrow(ds_clean), by=2)
tr.ds <- ds_clean[tr.idx, 2:ncol(ds_clean)]
test.ds <- ds_clean[-tr.idx, 2:ncol(ds_clean)]
cluster.tr <- factor(ds_clean[tr.idx, 1])
cluster.test <- factor(ds_clean[-tr.idx, 1])

#모델 적용
pred_3 <- knn(tr.ds, test.ds, cluster.tr, k=3, prob=T)
acc_3 <- mean(pred_3==cluster.test)
acc_3

pred_5 <- knn(tr.ds, test.ds, cluster.tr, k=5, prob=T)
acc_5 <- mean(pred_5 == cluster.test)
acc_5

pred_7 <- knn(tr.ds, test.ds, cluster.tr, k=7, prob=T)
acc_7 <- mean(pred_7 == cluster.test)
acc_7

#k 개수에 따른 결과 비교
k <- c(3,5,7)
acc <- c(acc_3, acc_5, acc_7)
data.frame(k=k, Accuracy= acc)
acc


#06
library(cvTools)
head(Glass)

#5-fold
k = 5
folds <- cvFolds(nrow(Glass), K=k)

acc <- c()
for (i in 1:k){
  ts.idx <- folds$which == i
  ds.tr <- Glass[-ts.idx, 1:8]
  ds.ts <- Glass[ts.idx, 1:8]
  cl.tr <- factor(Glass[-ts.idx, 9])
  cl.ts <- factor(Glass[ts.idx, 9], levels = levels(cl.tr))
  
  pred <- knn(ds.tr, ds.ts, cl.tr, k=3)
  acc[i] <- mean(pred==cl.ts)
}
acc
mean(acc)


#07
data("BreastCancer")
head(BreastCancer)

# 전처리: 결측값 제거 + ID 제거
ds <- ds[complete.cases(ds), ]
ds <- ds[, -1]  # ID 열 제거
head(ds)

# 입력 변수(1~9열)를 모두 numeric으로 변환
ds[, 1:9] <- lapply(ds[, 1:9], function(x) as.numeric(as.character(x)))

# 종속변수(Class)는 factor로 유지
ds$Class <- factor(ds$Class)

# K-fold
k <- 10
folds <- cvFolds(nrow(ds), K = k)
acc <- c()

# 교차검증
for (i in 1:k){
  ts.idx <- which(folds$which == i)
  
  ds.tr <- ds[-ts.idx, 1:9]
  ds.ts <- ds[ts.idx, 1:9]
  cl.tr <- ds[-ts.idx, 10]
  cl.ts <- factor(ds[ts.idx, 10], levels = levels(cl.tr))
  
  pred <- knn(ds.tr, ds.ts, cl.tr, k = 3)
  acc[i] <- mean(pred == cl.ts)
}

# 결과 출력
acc
mean(acc)