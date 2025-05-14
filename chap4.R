#chap4 조건문, 반복문, 함수

#조건문
job.type <-'A'

if(job.type == 'B'){
  bonus <- 200
} else {
  bonus <- 100
}
print(bonus)
job.type <- 'B'
bonus <- 100

if(job.type == 'A'){
  bonus <- 200
}

bonus

#다중 if
score <- 85

if (score>90){
  grade <- 'A'
} else if (score > 80) {
  grade <-'B'
} else if (score > 70) {
  grade <- 'C'
} else {
  grade <- 'D'
}

print(grade)

#논리연산자 사용
a<- 10
b<- 20

if (a>5 & b>5) {
  print(a+b)
}

if (a>6| b>30) {
  print(a*b)
}

#ifelse문
if (a>b) {
  c<-a
} else { #else의 위치 중요
  c<-b
}

print(c)

c <- ifelse(a>b, a, b)
print(c)

##반복문
#for문
for (i in 1:5){
  print('*')
}

for(i in 6:10) {
  print('*')
}

for(i in 1:9){
  cat('2 *', i, '=', 2*i, '\n') #한 줄에 여러개 결과값 출력
}

for(i in 1:20){
  if(i%%2==0){
    print(i)
  }
} #2로 나눈 나머지가 0이면 출력

sum <- 0
for(i in 1:100){
  sum <- sum + i
}
print(sum)

#꽃잎 길이 분류
norow <- nrow(iris)
mylabel <- (c)
for(i in 1:norow){
  if (iris$Petal.Length[i] <=1.6){
    mylabel[i] <- 'H'
  } else if (iris$Petal.Length[i] >=5.1){
    mylabel[i] <- 'L'
  } else {
    mylabel[i] <- 'M'
  }
}
print(mylabel)
newds <- data.frame(iris$Petal.Length, mylabel)
head(newds)


#while문
sum <- 0
i<-1
while(i<=100){
  sum <- sum+i
  i <- i+1 #무한루프에 빠지지 않도록 값 조정 필요
}

print(sum)

#break와 next
sum <- 0
for (i in 1:10) {
  sum <- sum + i
  if (i >=5) break
}
sum

for(i in 1:10){
  if (i%%2==0) next
  sum <- sum +i
}
sum


##apply 함수 데이터프레임인 경우 요걸 많이 씀
apply(iris[,1:4], 1, mean) #행방향
apply(iris[,1:4], 2, mean) #열방향

##사용자 정의 함수: 내가 만드는 함수
mymax <- function(x,y){
  num.max <- x
  if (y >x) {
    num.max <-y
  }
  return(num.max)
}

mymax(10,15)

mydiv <- function(x, y=2){
  result <- x/y
  return(result)
}

mydiv(10)

myfunc <- function(x, y) {
  val.sum <- x+y
  val.mul <- x*y
  return (list(sum=val.sum, mul=val.mul))
}

myfunc(5,8)

## 조건에 맞는 데이터 위치 찾기
score <- c(76, 23, 45, 67, 59, 96, 71, 85, 24, 96)
which(score==76)
which(score>-85)
max(score)
min(score)
which.max(score)
which.min(score)
idx <- which(score<=60)
score[idx] <- 61
score
score.high <- subset(score, score>=80)
score.high

idx <- which(iris$Petal.Length>5.0)
idx
iris.big<- iris[idx,]
iris.big

idx <- which(iris[,1:4]>5.0, arr.ind = T)
idx

#연습문제
#(1)
nums = 0
cnt = 0
for(i in 1:100){
  if(i%%3==0){
    nums <- nums + i
    cnt <- cnt + 1
  }
}
print(c(nums, cnt))
#(2)
for(i in 101:200){
  if(i%%12==0){
    print(i)
  }
}
#(3)
for(i in 1:24){
  if(24%%i==0){
    print(i)
  }
}
#(4)
fac <- 1
for(i in 1:10){
  fac <- fac * i
}
print(fac)

#2
for(i in 1:6){
  cat(strrep("*", i), "\n")
}

#3
i<-1
num <- 0
sum <- 0
while(i<10){
  num <- i*i
  sum <- sum + num
  i <- i + 1
}
print(sum)

#4
score <- 80
result <- ifelse(score>60, 'Pass', 'Fail')
print(result)

#5
nums <- 2:1000
prime <- c()
while (length(nums)>0) {
  p<- nums[1]
  prime <- c(prime, p)
  nums<- nums[nums %%p !=0]
}
print(prime)

#6
numlist <- c(0,1)
while (length(numlist)<40){
  next_num <- numlist[length(numlist)] + numlist[length(numlist)-1]
  numlist <- c(numlist, next_num)
}
print(numlist)

#7
#(1), (2)
apply(iris[,1:4], 1, sum)
apply(iris[,1:4], 2, max)

#8
#(1)
apply(mtcars, 2, sum)
apply(mtcars, 2, max)
apply(mtcars, 2, sd)

#09 
lgm <- function(x, y){
  while(y !=0){
    temp <- y
    y <- x %% y
    x <- temp
  }
  return(x)
}
lgm(10, 8)
lgm(10, 20)

#10
maxmin <- function(v) {
  maxnum<- which.max(v)
  minnum<-which.min(v)
  
  return (list(max=v[maxnum], min=v[minnum]))
}

v1 <- c(7,1,2,8,9)
result <- maxmin(v1)
result$max
result$min

#11
#(1,2,3,4)
weight <- c(69, 50, 55, 71, 89, 64, 59, 70,71,80)
which.max(weight)
which.min(weight)
which(weight < 69 & weight > 61)
weight.2 <- subset(weight,weight <= 60)
weight.2

#12
#(1)
idx<- which.max(mtcars$mpg)
mtcars[idx,]
#(2)
idx2<-which(mtcars$wt >2 & mtcars$wt <3)
mtcars[idx2,]
#(3)
idx3<- which(mtcars$gear ==3 )
apply(mtcars[idx3,], 2, mean)
#(4)
idx4<-which(mtcars$wt >=5)
apply(mtcars[idx4,], 2, max)
