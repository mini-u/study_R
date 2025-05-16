#chap6 다중변수 자료의 탐색
#산점도
wt <- mtcars$wt
mpg <- mtcars$mpg

plot(wt, mpg,
     xlab='중량',
     ylab='연비',
     col='red',
     pch=19)


vars <- c("mpg","disp", "drat", "wt")
target <- mtcars[,vars]
head(target)
pairs(target,
      main='멀티플롯')

iris.2 <- iris[,3:4]
point <- as.numeric(iris$Species)
point
color <- c("red", "orange", "blue")
plot(iris.2,
     main="제목",
     pch=c(point),
     col=color[point])


#상관분석과 상관계수
beers <- c(5,2,9,8,3,7,3,5,3,5)
bal <- c(0.1, 0.03, 0.19, 0.12, 0.03, 0.0095, 0.07, 0.06, 0.02, 0.05)
tbl <- data.frame(beers, bal)
tbl
plot(bal~beers, data=tbl)
res <- lm(bal~beers, data=tbl)
abline(res) #회귀선 작성
cor(beers, bal)

cor(iris[,1:4]) #변수간 상관계수


#선그래프
month= 1:12
late = c(5,8,7,9,4,6,7,8,9,10,11,3)
plot(month,
     late,
     main='지각생 통계',
     type = "o",
     lty=6, 
     lwd=5)
late2 = c(7,8,9,10,11,3,1,2,3,4,5,6)
lines(month,
      late2,
      type='b',
      col='red')


#EDA
install.packages('mlbench')
library(mlbench)
data("BostonHousing")
myds <- BostonHousing[,c("crim", "rm", "dis", "tax", "medv")]

#그룹변수 추가
grp <- c()
for (i in 1:nrow(myds)){
   if (myds$medv[i] >=25.0) {
     grp[i] <- "H"
   } else if (myds$medv[i]<=17.0){
     grp[i] <- "L"
   } else {
     grp[i] <- "M" 
     }
}

grp <- factor(grp)
grp <- factor(grp, levels=c("H", "M", "L"))
myds <- data.frame(myds, grp)

myds


#데이터셋 형태 파악
str(myds)
table(myds$grp)

par(mfrow=c(2,3))
for (i in 1:5){
  hist(myds[, i], main=colnames(myds)[i], col='yellow')
}
par(mfrow=c(1,1))

par(mfrow=c(2,3))
for (i in 1:5){
  boxplot(myds[,i])
}
par(mfrow=c(1,1))

#그룹별 관측값 분포 확인
boxplot(myds$crim~myds$grp)
boxplot(myds$rm~grp)

pairs(myds[,-6])


point <- as.integer(myds$grp)
color <- c("red", "orange", "blue")
pairs(myds[,-6], pch=point, col=color[point])

cor(myds[,-6])


##실전분석
tmp <- airquality[,1:4]
tmp <- tmp[complete.cases(tmp),]
head(tmp)
plot(tmp)
cor(tmp)
cor(tmp$Ozone, tmp$Temp)
plot(tmp$Ozone, tmp$Temp, main='제목')
res <- lm(tmp$Temp~tmp$Ozone)
abline(res, col='red')


##연습문제
#01
cor(cars$speed, cars$dist)
plot(cars$speed, cars$dist)
res <- lm(cars$dist~cars$speed)
abline(res, col='red')
#상관계수가 0.8로 강한 양의 상관관계임

#02 
cor(pressure$temperature, pressure$pressure)
plot(pressure$temperature, pressure$pressure)
res <- lm(pressure$pressure~pressure$temperature)
abline(res, col='red')
#상관계수가 0.76으로 양의 상관관계가 있는 편임

#03
ds <- swiss[,1:4]
head(ds)
pairs(ds)

#04
plot(iris$Petal.Length, iris$Petal.Width,
     col = iris$Species,
     pch = 3)

#07
ds <- state.x77
cor(ds)
pairs(ds)
#Income과 가장 큰 상간관계를 가진변수는 HS Grad임

#10
#(1)
plot(trees$Girth, trees$Height)
cor(trees$Girth, trees$Height)
#(2)
pairs(trees)
cor(trees)

#11
plot(Orange$age, Orange$circumference,
     col=Orange$Tree,
     pch = 10)

#12
#(1)
data("Glass")
myds <- Glass
#(2,3,4)
pairs(myds[,-10])
cor(myds[,-10])
pairs(myds[,-10],
      col = myds$Type)

#14
years <- 1875:1972
hlevel <- as.vector(LakeHuron)
lines(years,
     hlevel,
     type='b',
     main = 'LakeHuron',
     col='skyblue')

#16 
str(USAccDeaths)
months <- 1:12
acc_1973 <- acc[1:12]
acc_1975 <- acc[25:36]
acc_1977 <- acc[49:60]

plot(months, acc_1973, type = "l", col = "red")
lines(months, acc_1975, col = "blue")
lines(months, acc_1977, col = "green")