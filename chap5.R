# 단일변수 범주형 자료의 탐색

# 1. 도수분포표 작성 

favorite <- c("겨울", "봄", "가을", "가을", "봄", "여름", "겨울", "겨울", "겨울")
favorite
table(favorite)
table(favorite)/length(favorite)

# 2. 막대그래프 작성

df <- table(favorite)
df
barplot(df, main='favorite season')

df <- df[c(3,4,1,2)] #순서바꾸기
df

# 3. 원그래프
pie(df, main='season')

# 4. 숫자로 표현된 범주형 자료
favorite_color <- c(2,3,2,1,1,2,2,3,2,3,1,2,3,2,2,2,1,1)
df_col <-table(favorite_color)
df_col
barplot(df_col, main="color")
colors <- c('green', 'red', 'blue')
names(df_col) <- colors #자료의 이름 변경
df_col
barplot(df_col, col=colors)
pie(df_col, col=colors)

###############################################
#단일변수 연속형 자료 탐색

#1. 평균과 중앙값
weight <- c(60, 50, 49, 59, 60, 65, 56, 62)
weight.heavy<-c(weight, 120)
weight
weight.heavy

mean(weight)
mean(weight.heavy)

median(weight)
median(weight.heavy)

mean(weight, trim=0.2) #절사평균
mean(weight.heavy, trim=0.2)


#2. 사분위수
mydata <- c(50, 62, 64, 66, 67, 67, 68, 69, 130)
quantile(mydata)
quantile(mydata, (0:10)/10)  #10%단위로 구간 나눠 계산 오 이건 편하다
summary(mydata)


#3. 산포
var(mydata) #분산
sd(mydata) #표준편차
range(mydata) #값의 범위
diff(range(mydata)) #최대 최소 차이 


#4. 히스토그램
dist <- cars[,2]
hist(dist,
     main ='제동거리 히스토그램',
     xlab = '제동거리',
     ylab = '빈도',
     border = 'blue', #테두리색
     col = 'pink', #막대색
     las = 1, #x축글씨방향
     breaks=5) #구간 수


#5.상자그림
boxplot(dist, main='제동거리')
boxplot.stats(dist)

#6. 그룹이 있는 자료의 상자 그림
boxplot(Petal.Length ~ Species, data=iris) #length를 ~species별로 나누어 그리라는 소리


#가상화면 분할 그래프 그리기
par(mfrow=c(1,3)) #가상화면 분할 (행, 열)
barplot(table(mtcars$carb),
        main='분할',
        xlab = "#of carburetors",
        ylab = "frequency",
        col = 'blue')
barplot(table(mtcars$cyl),
        main='분할2',
        xlab = "#of cylender",
        ylab = "frequency",
        col = 'red')
barplot(table(mtcars$gear),
        main='분할3', 
        xlab = "#of gears",
        ylab = "frequency",
        col = "green")
par(mfrow=c(1,1)) #가상화면 분할 해제


# 실전분석
temp <- quakes
head(temp)

temp$group <- 'A'
temp$group[temp$long >=180] <- 'B'
head(temp)

boxplot(mag~group, data=temp, main='진도 분포')
boxplot(depth~group, data=temp)


#연습문제
#01
#(1, 2,3,4)
tabaco <- esoph$tobgp
tabaco
unique(tabaco)
table(tabaco)
barplot(table(tabaco), main='막대그래프')

#02
#(1,2,3,4)
result <- c("P", "P", "F", "P", "F", "P", "F", "P", "P", "F")
table(result)
barplot(table(result), main='막대그래프')
pie(table(result), main='파이차트')

#03
#(1,2,3,4)
season <- c("여름", "봄", "겨울", "가을", "여름", "여름", "가을", "겨울", "봄")
table(season)
barplot(table(season), main='계절선호', col=colors)
pie(table(season))

#05
ds <- mtcars
mean(ds$wt)
median(ds$wt)
mean(ds$wt, trim = 0.15)
sd(ds$wt)
summary(ds$wt)
table(ds$cyl)
boxplot(ds$wt)
boxplot.stats(ds$wt)
boxplot(ds$disp)
boxplot(ds$mpg~ds$gear)

#06
ds.tree <- trees
head(ds.tree)
mean(ds.tree$Girth)
median(ds.tree$Girth)
mean(ds.tree$Girth, trim=0.15)
sd(ds.tree$Girth)
boxplot(ds.tree$Girth, main="나무 지름")
hist(ds.tree$Height)
boxplot(ds.tree$Height)

#07 
ds.orange <- Orange
head(ds.orange)
summary(ds.orange$age)
mean(ds.orange$age, trim=0.15)
boxplot(ds.orange$age~ds.orange$Tree)
hist(ds.orange$circumference[ds.orange$Tree !=2])
boxplot(ds.orange$circumference~ds.orange$Tree)
