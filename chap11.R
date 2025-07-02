# 11. 회귀 분석

# 단순선형 회귀분석 
ds <- cars
head(ds)

plot(dist~speed, data=ds)

model <- lm(dist~speed, ds)
model #속도에 따른 제동거리 회귀식

abline(model)

b <- coef(model)[1]
w <- coef(model)[2]

speed <- 30
dist <- w*speed + b
dist #속도가 30일 때, 제동거리 100.39

speed <- ds[, 1]
pred <- w*speed +b
pred

compare <- data.frame(pred, ds[,2], pred-ds[,2])
head(compare)


# 다중선형 회귀분석
library(car)
head(Prestige)
newds <- Prestige[,c(1:4)]
plot(newds, pch=16, col='blue', main="Matrix scatterplot")

mod1 <- lm(income ~ education + prestige + women, data=newds)
summary(mod1)


# 다중선형 모델에서 변수의 선택
library(MASS)
newds2 <- Prestige[, c(1:5)]
head(newds2)

mod2<- lm (income ~ education + prestige + women +census, newds2)
mod3 <- stepAIC(mod2)
mod3
summary(mod3)
summary(mod2)


# 로지스틱 회귀 분석

iris.new <- iris
iris.new$Species <- as.integer(iris.new$Species)
head(iris.new)
mod.iris <- glm(Species ~ ., data=iris.new)
summary(mod.iris)
