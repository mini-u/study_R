# 1. 데이터 시각화

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