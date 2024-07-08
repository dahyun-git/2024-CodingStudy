### Week 2: Beyond Linearity ###

install.packages("ISLR")
library(ISLR)
attach(Wage)

# Wage: Wage and other data for a group of 3000 male workers in the Mid-Atlantic region.

### EDA ###

summary(Wage)
# year, age, marit1, race, education, region, jobclass, health, health_ins, logwage, wage

str(Wage)
head(Wage)

sum(is.na(Wage)) # 0

# 변수 분포 확인인
hist(age, main="Age Distribution", xlab="Age", breaks=30)
summary(age)
hist(wage, main="Wage Distribution", xlab="Wage", breaks=30)
summary(wage)

# 범주형 변수 분포 확인
## 교육수준
table(education)
barplot(table(education), main="Eduction Level Distribution", las=2)

## 결혼 상태
table(maritl)
barplot(table(maritl), main="Marital Status Distribution", las=2)

# 변수 간 관계 시각화
plot(age, wage, main="Age vs. Wage", xlab="Age", ylab="Wage", pch=19)
boxplot(wage ~ education, Wage, main="Wage by Education Level", xlab="Eduction Level", ylab="Wage", las=2)
boxplot(wage ~ jobclass, Wage, main="Wage by Job Class", xlab="Job Class", ylab="Wage")

# 상관관계 확인
numeric_vars <- Wage[, sapply(Wage, is.numeric)]
cor_matrix <- cor(numeric_vars)
cor_matrix # no high correlations found

# 시각화
library(ggplot2)
ggplot(Wage, aes(x=age, y=wage, color=education)) +
  geom_point(alpha=0.6) +
  labs(title="Age vs. Wage by Education Level", x="Age", y="Wage") +
  theme_minimal()

### General Linear Model ###
lm <- lm(wage ~ age)
summary(lm) # 0.70728

### Polynomials ###
pm <- lm(wage ~ poly(age, 4))
summary(pm) # 447.0679, -468, 3158, 125.5217, -77.9112

pm2 <- lm(wage ~ poly(age, 4, raw=T))
summary(pm2) # 2.125e+01, -5.639e-01, 6.811e-03, -3.204e-05

# poly()와 poly(..., raw=T)의 차이
# poly(age, 4)은 정규 직교 다항식을 사용해 age의 다항식 변환을 생성함
# 이 방법은 다항식 항들 사이의 상관성을 줄여 회귀모델의 수렴 속도를 높이고 해석을 더 용이하게 함
# 계수들은 직교 다항식의 축소된 형태로, 각 계수는 다른 계수의 영향을 받지 않고 독립적으로 변화를 설명함

# poly(age, 4, raw=T)은 원시 다항식을 사용해 age의 다항식 변환을 생성함
# 이는 우리가 일반적으로 생각하는 다항식
# 해석 예) poly(age, 4, raw=T)1: 21.25는 나이 한 단위 증가에 따른 임금의 선형 증가량
# 해석 예) poly(age, 4, raw=T)2: -0.5639는 나이의 제곱에 대한 계수로, 나이가 증가할수록 임금의 변화가 가속도적으로 감소하는 것을 의미

# 그렇다면 왜 polynomial을 사용?
# 비선형 관계, 고차원 데이터 등 모델링 할 때

pm3 <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4))
summary(pm3) # lm_poly2와 같음

# 시각화
agelims = range(age) # 18 to 80
age.grid <- seq(from=agelims [1], to=agelims [2])
age.grid # 18부터 80까지 같은 간격으로 나눔

preds <- predict(pm, newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands

par(mfrow=c(1, 2), mar=c(4.5, 4.5, 1, 1), oma=c(0, 0, 4, 0))
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
lines(age.grid, preds$fit, lwd=2, col="blue")    
title("Degree-4 Polynomial")

preds2 <- predict(pm2, newdata=list(age=age.grid), se=TRUE)
max(abs(preds$fit - preds2$fit)) # very small difference

### Step Functions ###
table(cut(age, 4)) #cut: age를 같은 간격으로 4개 세그먼트로 자름
sfm <- lm(wage ~ cut(age, 4))
summary(sfm)

### Splines ###
library(splines)

## cubic splines (default) ##

cs <- lm(wage ~ bs(age, knots=c(25, 40, 60)))
preds3 <- predict(cs, newdata=list(age=age.grid), se=T)
cs2 <- lm(wage ~ bs(age, df=6))
preds4 <- predict(cs2, newdata=list(age=age.grid), se=T)
plot(age, wage, col='gray')
lines(age.grid, preds3$fit, col="blue", lwd=2)
lines(age.grid, preds4$fit, col="red", lwd=2)

## natural splines ##

ns <- lm(wage ~ ns(age, df=4))
preds5 <- predict(ns, newdata=list(age=age.grid), se=T)
plot(age, wage, col='gray')
lines(age.grid, preds3$fit, lwd=2, col="blue")
lines(age.grid, preds5$fit, lwd=2, col="orange")
# at the boundaries, more straight bc they are designed to fit linear model at the boundaries

## smoothing splines ##

par(mfrow=c(1, 1))
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
ss = smooth.spline(age, wage, df=16)
ss2 = smooth.spline(age, wage, cv=TRUE) #cv로 df 선택
ss2$df # 6.8
lines(ss, col="red", lwd=2) #wiggly
lines(ss2, col="blue", lwd=2) # more robust compared to df=16
legend("topright", legend=c("df=16", "df=6.8"), col=c("red", "blue"), lty=1, lwd=2)

### Local Regression ###

plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
lg = loess(wage ~ age, span=.2)
# the wider the span is, the more dataset we include in regression
lg2 = loess(wage ~ age, span=.5)
lines(age.grid, predict(lg, data.frame(age=age.grid)),
      col="red", lwd=2)
lines(age.grid, predict(lg2, data.frame(age=age.grid)),
      col="blue", lwd=2)
# larger span => less wiggly evaluated functions
legend("topright", legend=c("span=0.2", "span=0.5"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)


### GAM ###

# using natural splines
gam <- lm(wage ~ ns(year, 4) + ns(age, 5) + education)
# non-linear covariate 써도 we can still use lm bc they are represented by basis covariate model
# education은 categorical variable => step function

install.packages("gam")
library(gam)

# using smoothing splines
gam2 = gam(wage ~ s(year, 4) + s(age, 5) + education)
par(mfrow=c(1, 3))

plot(gam2, se=TRUE, col="blue")
plot.Gam(gam, se=TRUE, col="red")

# mixing non-linear models
gam3 = gam(wage ~ s(year, df=4) + lo(age, span=0.7) + education)
plot.Gam(gam3, se=TRUE, col="green")