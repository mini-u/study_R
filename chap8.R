# 8. 데이터 시각화

#(2) 트리맵
install.packages('treemap')
library(treemap)
data(GNI2014)
head(GNI2014)
treemap(GNI2014,
        index =c("continent", "iso3"), #계층구조(대륙 - 국가)
        vSize = "population", #타일 크기는 인구 수
        vColor="GNI", #타일 컬러는 gni별로
        type ="value", #타일 컬러링은 값으로
        title= "월드 GNI"
        )

st <- data.frame(state.x77)
st <- data.frame(st, stname=rownames(st))
st

treemap(st,
        index=c("stname"),
        vSize = "Area",
        vColor = "Population",
        type = "value",
        title="usa states area and ppl") #알래스카는 면적 넓지만 인구 적음


#(3) 버블차트: 3개 변수 시각화 가능
symbols(st$Illiteracy, st$Murder, #x좌표, y좌표
        circles=st$Population, #버블크기
        inches=0.3, #크기 조절값
        fg="white", #원 테두리 색
        bg="lightgrey", #원 색
        lwd=1.5,  #원의 테두리 두께
        xlab = "rate of Illiteracy",
        ylab = "murder rate",
        main ="Illiteracy and Crime")
text(st$Illiteracy, st$Murder, #출력될 텍스트 좌표
     rownames(st), #출력할 텍스트
     cex=0.6, #폰트 크기
     col='brown') #폰트 컬러


#(4) 모자이크 플롯
head(mtcars)
mosaicplot(~gear+vs, data=mtcars, color=T,
           main="Gear and VS")
mosaicplot(~gear+vs, data=mtcars, color=c("green", "blue"),
           main="Gear and Vs")

tbl <- table(mtcars$gear, mtcars$vs)
tbl
mosaicplot(tbl, color=T)


# 2. ggplot 패키지

#(2) 막대그래프
library(ggplot2)

month <- c(1,2,3,4,5,6)
rain <- c(55, 50, 45, 50, 60, 70)
df <- data.frame(month, rain)
df

ggplot(df, aes(x=month, y=rain))+ #그래프 그릴 데이터 지정
  geom_bar(stat="identity", #막대 높이는 y축 값에 의해 결정
           width=0.7, #막대 폭
           fill ="steelblue") + #색
  ggtitle("월별 강수량") + #제목
  theme(plot.title = element_text(size=25, face="bold", colour = "steelblue")) + #제목스타일
  labs(x="월", y="강수량")+ #축 제목 설정
  coord_flip() #막대를 가로로

#(3) 히스토그램
ggplot(iris, aes(x=Petal.Length))+
  geom_histogram(binwidth=1) #히스토그램에서 막대 간격 조정

ggplot(iris, aes(x=Sepal.Width, fill=Species, color=Species))+ #히스토그램 대상열, 내부 색설정
  geom_histogram(binwidth=0.5, position ="dodge")+
  theme(legend.position = "top")

#(4) 산점도
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width))+
  geom_point()

ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color = Species ))+
  geom_point(size=2)+
  ggtitle("꽃잎의 길이와 폭")+
  theme(plot.title=element_text(size=25, face="bold"))

#(5) 상자그림의 작성
ggplot(iris, aes(y=Petal.Length))+ #상자 변수 그릴 때는 y에 작성
  geom_boxplot(fill="yellow")

ggplot(iris, aes(y=Petal.Length, fill=Species))+
  geom_boxplot()

#(6) 선 그래프 작성
year <- 1937:1960
cnt <- as.vector(airmiles)
df <- data.frame(year, cnt)
df

ggplot(df, aes(x=year, y=cnt))+
  geom_line(col="red")


##3. 차원 축소
install.packages("Rtsne")
library(Rtsne)

ds <- iris[,-5]

dup = which(duplicated(ds))
dup
ds<- ds[-dup,]
ds.y <- iris$Species[-dup]

tsne <- Rtsne(ds, dims=2, perplexity=10)

df.tsne <- data.frame(tsne$Y)
head(df.tsne)
ggplot(df.tsne, aes(x=X1, y=X2, color=ds.y))+
  geom_point(size =2)

install.packages(c("rgl","car"))
library("rgl")
library("car")
library("mgcv") 

tsne <- Rtsne(ds, dims=3, perplexity =10)
df.tsne <- data.frame(tsne$Y) 
head(df.tsne)

scatter3d(x=df.tsne$X1, y=df.tsne$X2, z=df.tsne$X3)

points <- as.integer(ds.y)
color <- c('red', 'green', 'blue')
scatter3d(x=df.tsne$X1, y=df.tsne$X2, z=df.tsne$X3,
          point.col=color[points],
          surface=FALSE)

# 실전분석
library(car)
library(ggplot2)
tmp <- SLID[complete.cases(SLID), ]
tmp

tmp$group <- 'Middle'
tmp$group[tmp$education >=14] <- 'High'
tmp$group[tmp$education < 10]<- 'Low'

tmp$group <- factor(tmp$group, levels =c("High", "Middle", "Low"))

ggplot(data=tmp, aes(y=wages, fill=group))+
  geom_boxplot()


#연습문제
#01
us <- data.frame(state.x77, state.division)
us
us$index <- rownames(us)
us
#(1)
library(treemap)
treemap(us, index=c("state.division", "index"),
        vSize="Population",
        vColor="Income",
        title="1-(1) 그래프")

#(2)
treemap(us, index=c("state.division", "index"),
        vSize="HS.Grad",
        vColor="Murder")

#(3)
symbols(us$Income, us$Illiteracy,
        circle=us$Population,
        bg="green")
text(us$Income, us$Illiteracy,
     rownames(us))

#(4)
symbols(us$Illiteracy, us$Murder,
        circle=us$Area,
        bg="green")
text(us$Illiteracy, us$Murder,
     rownames(us))



#02
ds <- data.frame(swiss)
ds$index <- rownames(ds)
ds
#(1)
ds$group <- "MID"
ds$group[ds$Education >=13] <- "HIGH"
ds$group[ds$Education <= 6] <- "LOW"

treemap(ds, index=c("group", "index"),
        vSize="Fertility",
        vColor="Agriculture")

#(2)
treemap(ds, index="index",
        vSize="Catholic", vColor = "Examination")


#03
symbols(ds$Fertility, ds$Agriculture,
        circle=ds$Education,
        bg="green")
text(ds$Fertility, ds$Agriculture,
     rownames(ds))

#04
ds_tree <- data.frame(trees)
head(ds_tree)

symbols(ds_tree$Girth, ds_tree$Height,
        circle=ds_tree$Volume,
        bg="blue")
text(ds_tree$Girth, ds_tree$Height,
     rownames(ds_tree))


#05
ds_color <- data.frame(HairEyeColor)
head(ds_color)
mosaicplot(~Hair+Eye, data=ds_color)

#07
library(ggplot2)
cars <- data.frame(mtcars)
head(cars)
ggplot(cars, aes(x=carb)) +
  geom_bar()+
  labs(
    x = "기화기의 수", 
    y = "빈도",
    title = "기화기의 수"
  )


#08
ggplot(cars, aes(x=cyl))+
  geom_bar(fill='green')


#09
ggplot(cars, aes(x=mpg))+
  geom_histogram(binwidth = 5.0)


#10
ggplot(ds_tree, aes(x=Volume))+
  geom_histogram(binwidth = 5.0, fill='steelblue')+
  labs(x="나무부피", y="빈도", title="나무부피 히스토그램")


#11
ggplot(ds, aes(x=Examination, y=Agriculture, color=Fertility))+
  geom_point()


#12
ggplot(cars, aes(y=mpg, fill=factor(gear)))+
  geom_boxplot()


#15
library(Rtsne)
ds <- data.frame(state.x77)
head(ds)

#2차원 산점도
tsne <- Rtsne(ds, dims=2, perplexity=10)
df.tsne <- data.frame(tsne$Y)
head(df.tsne)
ggplot(df.tsne, aes(x=X1, y=X2))+
  geom_point()

#3차원 산점도
library("rgl")
library("car")
tsne <-Rtsne(ds, dims=3, perplexity=10)
df.tsne <- data.frame(tsne$Y)
head(df.tsne)
scatter3d(x=df.tsne$X1, y=df.tsne$X2, z=df.tsne$X3)


#16
#2차원 산점도
tsne <- Rtsne(ds, dims=2, perplexity=10)
df.tsne <- data.frame(tsne$Y)
ggplot(df.tsne, aes(x=X1, y=X2))+
  geom_point()

#3차원 산점도
tsne <-Rtsne(ds, dims=3, perplexity=10)
df.tsne <- data.frame(tsne$Y)
head(df.tsne)
scatter3d(x=df.tsne$X1, y=df.tsne$X2, z=df.tsne$X3)
