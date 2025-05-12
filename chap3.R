#chap3 매트릭스와 데이터프레임

## 1. 매트릭스
z <- matrix(1:20, nrow=4, ncol=5)
z
z1 <- matrix(1:20, nrow=5, ncol=4, byrow=T)
z1

#벡터로 만들기
x <- 1:4
y <- 5:8

z <- matrix(1:20, nrow=4, ncol=5)

m1 <- cbind(x,y)
m1

m2 <- rbind(x,y)
m2

m3 <- rbind(m2, x)
m3

m4 <- rbind(m3, y)
m4

m5 <- cbind(z, x)
m5

#데이터프레임 만들기

city <-c("Seoul", "도쿄", "워싱턴")
rank <- c(1,3,2)
city.info <- data.frame(city, rank)
city.info

iris
iris[,c(1:2)] #행, 열로 슬라이싱
iris[,c(1,3,5)]
iris[1:5,]
iris[1:5, c(1:3)] #1:5행의 데이터 중 1열~3열의 데이터 가져오기

dim(iris) #행과 열의 개수 출력
nrow(iris) #행 개수 출력
ncol(iris) #열 개수 출력
colnames(iris) #컬럼 이름 출력
head(iris) #데이터셋 앞부분 출력
tail(iris) #데이터셋 뒷부분 출력
head(iris, 20)

str(iris) #데이터셋의 요약정보
iris[,5] #품종 데이터 보기,.?
iris[,3]
unique(iris[,5]) #유니크값만
table(iris[,"Species"]) #품종의 종류별 행의 개수 세기


colSums(iris[,-5]) #열별 합계
colMeans(iris[,-5]) #열별 평균
rowSums(iris[,-5]) #행별 합계
rowMeans(iris[,-5]) #행별 평균

#행열 전환
z <- matrix(1:20, nrow=4, ncol=5)
z
t(z)

#조건에 맞는 행과 열 추출
IR.1 <- subset(iris, Species=="setosa")
IR.1
IR.2 <- subset(iris, Sepal.Length>5.0 & Sepal.Width>4.0)
IR.2
IR.2[,c(2, 4)] #subset함수의 경우 데이터프레임에만 잘 적용됨


#산술연산
a <- matrix(1:20, 4,5)
b<- matrix(21:40, 4, 5)
a
b
2*a
b-5
a+b
b*a
b/a
a
a<-a*4
a
a <- a*3
a

#매트릭스와 데이터 프레임 자료 구조 확인
class(iris)
class(state.x77) #미국 50개 주에 대한 통계
is.matrix(iris)
is.matrix(state.x77)
is.data.frame(iris)

#매트릭스와 데이터 프레임의 자료구조 확인
st <- data.frame(state.x77)
head(st)
class(st)

iris.m <- as.matrix(iris[,1:4]) #iris 데이터셋에서 숫자만 포함
head(iris.m) 
class(iris.m)

#데이터 프레임의 열 추출
iris[,"Species"] #결과가 팩터값임. 매트릭스 데이터프레임 둘다 가능
iris[,5] #결과가 팩터
iris["Species"] #결과가 데이터 프레임
class(iris["Species"])
class(iris[5])
class(iris$Species)

#파일 데이터 읽고 쓰기, 디렉터리 세팅하기
setwd("D:\\MINNIE\\study_R\\모두를 위한 R데이터 분석")
my.iris <-subset(iris, Species =='setosa')
write.csv(my.iris, "my_iris.csv", row.names = F)


#연습문제
#2번
st <- data.frame(state.x77)
st
colnames(st)
rownames(st)
dim(st)
str(st)
colSums(st)
rowSums(st)
colMeans(st)
rowMeans(st)
st["Florida",]
st[, "Income"]
st["Texas","Area"]
st["Ohio", c(1,2)]
subset(st, Population >= 5000)
subset(st, Income >=4500, select= c("Population","Income", "Area"))
sum(st$Income>=4500)
subset(st, Area>=100000 & Frost<=120)
subset(st, Population <2000 & Murder <12)
mean(st$Income[st$Illiteracy >=2.0])
mean(st$Income[st$Illiteracy <=2.0]) - mean(st$Income[st$Illiteracy >=2.0])
subset(st, Life.Exp == max(st$Life.Exp))
subset(st, Income > st["Pennsylvania", "Income"])


#3번
name <- c("Tom", "Jane", "Nick")
age <- c(20, 23, 26)
gender <- c("M", "F", "M")
height <- c(180, 160, 175)
student <- c(TRUE, TRUE, FALSE)
human <- data.frame(name, age, gender, height, student)
human
new_row <- list (
  name = "Mary",
  age = 24,
  gender = "F", 
  height = 155,
  student = TRUE
)
human <- rbind(human, new_row)
human
str(human)
mean(human$age)
mean(human$height)
names(human)[-4]
table(human$gender)


#4번
class(airquality)
head(airquality)
subset(airquality, Temp == max(airquality$Temp))
subset(airquality, airquality$Month == 6 & Wind == max(airquality$Wind))
mean(airquality$Temp[airquality$Month == 7])
mean(airquality$Ozone[airquality$Month ==5], na.rm=T)
sum


#5번
str(swiss)
swiss[swiss$Agriculture == max(swiss$Agriculture), ]
swiss[order(swiss$Agriculture, decreasing = T),]
subset(swiss$Agriculture, swiss$Catholic >=80)
subset_data <- subset(swiss, Examination < 20 & Agriculture < 50)
data.frame(
  Region = rownames(subset_data),
  Examination = subset_data$Examination,
  Agriculture = subset_data$Agriculture
)

