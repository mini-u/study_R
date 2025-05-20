##7. 데이터 전처리
#1 결측값

z <- c(1, 2,3,NA, 5, NA, 8)
sum(z)
is.na(z)
sum(is.na(z))
sum(z, na.rm=T) #결측치 제외하고 합 계산


#대체 및 제거 
z2 <- c(5,8,1,NA, 3, NA, 7)
z[is.na(z)] <-0 # NA를 0으로 치환
z
z3 <- as.vector(na.omit(z2)) #NA를 제거하고 새로운 벡터 생성
z3

x <- iris
x[1,2]<-NA;x[1,3]<- NA
x[2,3]<-NA;x[3,4]<-NA
head(x)

for (i in 1:ncol(x)){
  this.na <- is.na(x[,i])
  cat(colnames(x)[i], "\t", sum(this.na), "\n")
}

col_na <- function(y){
  return(sum(is.na(y)))
}

na_cnt <- apply(x, 2, FUN=col_na)
na_cnt

rowSums(is.na(x))
sum(rowSums(is.na(x)>0))
sum(is.na(x))

head(x)
x[!complete.cases(x),]
y <- x[complete.cases(x),]
head(y)

# 추정 시 mice패키지를 사용하기도 함

#2 특이값
st <-data.frame(state.x77)
boxplot(st$Income)
boxplot.stats(st$Income)$out #아웃라이어만 필터링

# 행 제거
out.val <- boxplot.stats(st$Income)$out #특이값을 추출해서 out.val에 저장
st$Income[st$Income %in% out.val]<-NA #income 값 중 outval에 있다면 na로 저장
head(st) 
newdata<- st[complete.cases(st),] #bna값이 있다면 na행 제거
head(newdata)


#3 데이터 정렬
v1 <-c(1,7,6,8,4,2,3)
order(v1)
v1 <- sort(v1)
v1
v2 <- sort(v1, decreasing = T) #내림차순

#매트릭스와 데이터 프레임 정렬
head(iris)
order(iris$Sepal.Length)
iris[order(iris$Sepal.Length),] #오름차순으로 정렬
iris[order(iris$Sepal.Length, decreasing = T),]

iris[order(iris$Species, -iris$Petal.Length, decreasing = T),]


#4 데이터 분리와 선택
sq <- split(iris, iris$Species)
sq
summary(sq)
sq$setosa

subset(iris, Species =='setosa')
subset(iris, Sepal.Length > 7.5)
subset(iris, Sepal.Length > 7.6,
       select = c(Petal.Length, Petal.Width))

#5 데이터 샘플링과 조합
x <- 1:100
y <- sample(x, size=10, replace=F)
y

idx <- sample(1:nrow(iris), size = 50,
              replace=F)
iris.50 <- iris[idx, ]
iris.50
dim(iris.50)

#set.seed
sample(1:20, size = 5)
sample(1:20, size = 5)
set.seed(100)
sample(1:20, size =5)
set.seed(100)
sample(1:20, size =5)

#데이터 조합
combn(1:5, 3)# 1부터 5까지 3개를 뽑는 조합
x = c("red", "green", "blue", "skyblue", "yellow", "purple")
com <- combn(x, 2)
com

for(i in 1:ncol(com)){
  cat (com[,i], " \n")
}


#6 데이터 집계와 병합
agg <- aggregate(iris[,-5], by=list(품종 = iris$Species),
                 FUN = mean)
agg #품종별로 평균 집계

agg <- aggregate(iris[,-5], by=list(품종 = iris$Species),
                 FUN = sd)
agg #품종별 표준편차

agg <- aggregate(mtcars, by=list(cyl=mtcars$cyl,vs=mtcars$vs), FUN=max)
agg

#병합
x<- data.frame(name=c("a", "b", "c"), math=c(90, 80, 40))
y<- data.frame(name=c("a", "b", "c"), korean = c(40, 50, 90))
x
y
z <- merge(x, y, by=c("name"))
z
z<- merge(x, y, all=T)
z


#########
# 실전분석
library(mlbench)
data("Vehicle")
head(Vehicle)

agg <- aggregate(Vehicle[, 'Max.L.Ra'], by=list(차량타입=Vehicle$Class),
                 FUN = mean)
agg

barplot(agg$x, names.arg = agg$차량타입, main='차량유형별 종횡비')


#########
#연습문제
#01
ds <- mtcars
ds[2,3] <- NA; ds[3,1]<- NA; ds[2,4] <- NA; ds[4,3]<-NA
#(1)
sum(is.na(ds))
#(2)
ds[!complete.cases(ds),]
#(3)
sum(rowSums(is.na(ds)>0))
#(4)
ds.new <- ds[complete.cases(ds), ]
ds.new

#02
mt <- data.frame(mtcars)
#(1)
boxplot(mt)
#(2)
mt2 <- as.data.frame(lapply(mt, function(x) {
  if (is.numeric(x)) {
    out.val <- boxplot.stats(x)$out
    x[x %in% out.val] <- NA
  }
  return(x)
}))
mt2
#(3)
mt2 <- mt2[complete.cases(mt2), ]
mt2


#03
#(1)
AQ <- airquality
AQ
#(2)
for (i in 1:ncol(AQ)){
  this.na <- is.na(AQ[,i])
  cat(colnames(AQ)[i], "\t", sum(this.na), "\n")
}
#(3)
rowSums(is.na(AQ))
#(4)
AQ[!complete.cases(AQ),]
#(5)
AQ2 <- as.data.frame(
  apply(AQ, 2, function(col) {
    if (is.numeric(col)) {
      col[is.na(col)] <- mean(col, na.rm = TRUE)
    }
    return(col)
  })
)
AQ2


#04
#(1)
ds <-data.frame(state.x77)
ds[order(ds$Population, decreasing = F), ]
#(2)
ds[order(ds$Income, decreasing= T), ]
#(3)
colnames(ds)[1] <- "state"
head(ds[order(ds$Illiteracy, decreasing = F), c("state", "Illiteracy")], 10)

#05
#(1)
mt.gear <- split(mtcars, mtcars$gear)
mt.gear
#(2)
mt.gear$'4'
#(3)
mt.gear.35 <- rbind(mt.gear[["3"]], mt.gear[["5"]])
mt.gear.35
#(4)
subset(mtcars, mtcars$wt > 1.5 & mtcars$wt < 3.0)

#06
#(1)
library(mlbench)
data("Glass")
myds <- Glass
#(2)
str(myds) #10번째 열은 팩터
aggregate(myds[, -10], by=list(Type = myds$Type), FUN=mean)

#07
#(1)
data("Ionosphere")
myds <- Ionosphere
str(myds)
#(2)
num_cols <- sapply(myds[,-35], is.numeric)
aggregate(myds[ , num_cols], by = list(class = myds$Class, V1 = myds$V1), FUN = sd)

#08
set.seed(100)
idx <- sample(1:nrow(mtcars), size=10, replace=F)
mt10 <- mtcars[idx,]
mt10
mt.other <- mtcars[-idx,]
mt.other

#09
set.seed(100)
idx <- sample(1:nrow(iris), size=10, replace=F)
iris.10 <- iris[idx,]
iris.10
