### Week 3: Tree Ensemble ###

## Decision Trees ##

# install.packages("tree")
# install.packages("ISLR2")
library(tree)
library(ISLR2)
?Carseats
attach(Carseats)
head(Carseats)
# Here we make a prediction on Sales

# Converting Sales to a binary variable
# 8 이하는 No, 8 초과는 Yes

High <- factor(ifelse(Sales<=8, "No", "Yes"))

Carseats <- data.frame(Carseats, High)
head(Carseats)

tree.carseats <- tree(High ~ . - Sales, Carseats)
# used all variables except for the Sales
# fitting a tree model
# High itself has been made using the Sales

summary(tree.carseats)
# Misclassification error rate: 0.09 => training error

## visualization
plot(tree.carseats)
text(tree.carseats, pretty=0)
# SelveLoc is making the biggest difference among the response variable

tree.carseats

## Prediction
set.seed(2)
train <- sample(1:nrow(Carseats), 200) # 숫자 200개 랜덤샘플링
Carseats.test <- Carseats[-train, ] # 테스트셋
High.test <- High[-train]

tree.carseats <- tree(High ~ . -Sales, Carseats, subset=train)
tree.pred <- predict(tree.carseats, Carseats.test, type="class")
# type="class": we have actual labels of responses rather than numerical value
table(tree.pred, High.test)
(104+50)/200 # 0.77: correct predictions
(33+13)/200 # 0.23: error rate

## Choosing a proper size of tree
# larger tree -> more complicated model -> overfitting
# 그래서, not too small and not too big at the same time
# using cross-validation

set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats) # size, dev, k, method
cv.carseats
# the most proper tree size = 9
par(mfrow=c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
# elbow point: size 9일 때
plot(cv.carseats$k, cv.carseats$dev, type="b")

## Fitting the training set using the chosen size
prune.carseats <- prune.misclass(tree.carseats, best=9)

plot(prune.carseats)
text(prune.carseats, pretty=0)
# more effective on making branches is Price

## Prediction
tree.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(97+58)/200 # 0.775 => slight improvement but much easier to interpret

## Trying some different number of tree size
prune.carseats <- prune.misclass(tree.carseats, best=14)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(102+52)/200 # 0.77

## Fitting regression trees
data(Boston)
head(Boston) # predicting medv, which means the median value of owner-occupied homes in $1000s

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~ ., Boston, subset=train)
summary(tree.boston)
# first split: rm (size of the room)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")
# larger dev => larger residuals => worse fit
# smallest when size 7

prune.boston <- prune.tree(tree.boston, best=5) # for simplicity, 5 instead of 7
plot(prune.boston)
text(prune.boston, pretty=0)

par(mfrow=c(1, 1))

## Prediction
yhat <- predict(tree.boston, newdata=Boston[-train, ])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test) # fitted value vs. actually observed value
# yhat = average of each node -> discrete value of yhat -> vertical lines
abline(0, 1)
# diagonal line => the more tightly the points are concentrated around the line, the better the fit becomes
mean((yhat-boston.test)^2) # test MSE = 35.28688



## Bagging and Random Forests

#install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv ~ ., Boston, subset=train, mtry=12, importance=TRUE)
# mtry=12: choose the number of covariate included in each split in the randomforest process
# random forest <- at each split, we choose n # of covariates randomly, n = preset # of covariates used at each step
# n<=p. n이 작을수록 less correlated that each tree is involved?
# m=p면 bagging 방법이랑 똑같아짐. 왜냐하면 randomness 없어지니까
# Boston 데이터의 변수 12개, 즉 p=12니까 mtry=12로 하면 bagging 하는 것
bag.boston

## test RSS
yhat.bag <- predict(bag.boston, newdata=Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
# better result compared to the single-tree case
mean((yhat.bag-boston.test)^2) # test MSE = 23.41916
# --> substantial improvement

## Choosing the number of trees
bag.boston <- randomForest(medv ~ ., Boston, subset=train, mtry=12, ntree=25)
yhat.bag <- predict(bag.boston, newdata=Boston[-train, ])
mean((yhat.bag-boston.test)^2) # 25.75055

## Setting mtry=6
set.seed(1)
rf.boston <- randomForest(medv ~ ., Boston, subset=tran, mtry=6, importance=TRUE)
# 변수 12개 중 반만 쓰는 것
yhat.rf <- predict(rf.boston, newdata=Boston[-train, ])
mean((yhat.rf-boston.test)^2) # 20.06644: decreased even further by random covariates

## Evaluating importance of each covariate
importance(rf.boston)
# IncMSE: average reduction in deviance by excluding each variable
# IncNodePurity: overall reductions

## Drawing the variance plot
varImpPlot(rf.boston)
# rm and lstat seem to affect the response the most out of variables



## Boosting
# install.packages("gbm")
library(gbm)
set.seed(1)
boost.boston <- gbm(medv ~ ., Boston[train, ], 
                    distribution="gaussian", n.trees=5000, interaction.depth=4)
# distribution="gaussian": we  want to perform regression
# distribution="binomial"이면 classification
# n.trees=5000: number of trees we are going to involve in a sequential manner
# interaction.depth: a depth of one tree

summary(boost.boston)
# importance of each variable

plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")
# a marginal effect of indicated variable on response
# rm => value increases; its marginal effect increases => positive coefficient in a linear model
# lstat =? value decreasees; its marginal effect decreases => negative coefficient in a linear model

## Prediction
yhat.boost <- predict(boost.boston, newdata=Boston[-train, ], n.trees=5000)
mean((yhat.boost-boston.test)^2) # 18.39057: smallest so far

## Tuning parameter
boost.boston <- gbm(medv ~ ., Boston[train, ],
                    distribution="gaussian", n.trees=5000,
                    interaction.depth=4, shrinkage=0.2, verbose=F)
yhat.boost <- predict(boost.boston, newdata=Boston[-train, ], n.trees=5000)
mean((yhat.boost-boston.test)^2) # 16.54778: further reduction compared to the previous model
# => boosting results can differ on the choice of shrinkage parameter as well