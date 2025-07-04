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

unknown <- data.frame(rbind(c(5.1, 3.5, 1.4, 0.2)))
names(unknown) <- names(iris)[1:4]
unknown

pred <- predict(mod.iris, unknown)
pred
round(pred, 0 )

levels(iris$Species)

test <- iris[, 1:4]
pred <- predict(mod.iris, test)
pred <- round(pred, 0)
pred
answer <- as.integer(iris$Species)
pred == answer
acc <- mean (pred ==answer)
acc
acc


# 실전 분석
library(boot)
head(survival)
cor(survival$dose, survival$surv)

model <- lm(surv~dose, data=survival)
model

plot(surv~dose, data=survival)
pred <- predict(model, survival['dose'])
pred

MAE <- sum(abs(pred-survival$surv))/nrow(survival)
MAE


## 연습문제
#1
head(Loblolly)
mod1 <- lm(height~age, data=Loblolly)
mod1

b <- coef(mod1)[1]
w <- coef(mod1)[2]

age <- c(10, 15, 20)
height <- w * age + b
height


#2
head(airquality)
mod1 <- lm(Temp~Wind, data=airquality)
mod1

b <- coef(mod1)[1]
w <- coef(mod1)[2]

wind <- c(10, 15, 20)
temp <- w * wind + b
temp


#3
head(pressure)
mod1 <- lm(pressure~temperature, data=pressure)
mod1

b <- coef(mod1)[1]
w <- coef(mod1)[2]

temp <- c(65, 95, 155)
press <- w * temp + b
press


#4
head(state.x77)
ds <- data.frame(state.x77)
head(ds)
#(1)
mod2 <- lm(Income~Illiteracy + HS.Grad, data=ds)
mod2

#(2)
mod3 <- lm(Income~Illiteracy+Population, data=ds)
mod3
pred <- predict(mod3, ds)
pred

#(3)
compare <- data.frame(pred, ds[,2], pred-ds[,2])
compare



#5
library(mlbench)
data("BostonHousing")
ds <- BostonHousing[, -4]
head(ds)

#(1)
mod <- lm(medv~., data=ds)
summary(mod)

#(2)
library(MASS)
mod1 <- stepAIC(mod)
summary(mod1)

#(3)
summary(mod)
summary(mod1)
#설명력이 0.7299로 같으나 (2)번의 경우 변수가 조금 더 적게 사용됨


#6
head(mtcars)

#(1)
mod <- lm(mpg ~ ., data=mtcars)
summary(mod)

#(2)
mod1 <- stepAIC(mod)
summary(mod2)


#9
ds <- Glass
head(ds)
str(ds)

#(1)
set.seed(100)
n <- nrow(ds)
train_index <- sample(1:n, size=round(0.6*n)) #전체의 60% 무작위 추출
train <- ds[train_index,]
test <- ds[-train_index, ]

#(2)
train$Type <- as.integer(train$Type)
mod <- glm(Type ~ ., data=train)
summary(mod)
mod

#(3)
test_ds <- test[,1:9]
pred <- predict(mod, test_ds)
round(pred, 0)

#(4)
round(pred, 0) == test[, 10]
acc <- mean(round(pred, 0) == test[,10])
acc
