library(MASS)
library(randomForest)

set.seed(1101)
# Construct the train and test matrices
train = sample(dim(Boston)[1], dim(Boston)[1]/2)
X.train = Boston[train, -14]
X.test = Boston[-train, -14]
Y.train = Boston[train, 14]
Y.test = Boston[-train, 14]

p = dim(Boston)[2] - 1
p.2 = p/2
p.sq = sqrt(p)

rf.boston.p = randomForest(X.train, Y.train, xtest = X.test, 
    ytest = Y.test,  mtry = p, ntree = 500)
rf.boston.p.2 = randomForest(X.train, Y.train, 
 xtest = X.test, ytest = Y.test,  mtry = p.2, ntree = 500)
rf.boston.p.sq = randomForest(X.train, Y.train,
xtest = X.test, ytest = Y.test, mtry = p.sq, ntree = 500)

plot(1:500, rf.boston.p$test$mse, col = "green", type = "l", 
     xlab = "Number of Trees", 
    ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston.p.2$test$mse, col = "red", 
      type = "l")
lines(1:500, rf.boston.p.sq$test$mse, col = "blue", 
      type = "l")
legend("topright", c("m=p", "m=p/2", "m=sqrt(p)"), 
       col = c("green", "red", "blue"), 
    cex = 1, lty = 1)

#The plot shows that test MSE for single tree is quite high (around 18). 
#It is reduced by adding more trees to the model and stabilizes around 
#a few hundred trees. Test MSE for including all variables at split is 
#slightly higher(apprx 11) as compared to both using half or square-root 
#number of variables (both slightly less than 10).

library(ISLR)
attach(Carseats)
set.seed(1)

train = sample(dim(Carseats)[1], dim(Carseats)[1]/2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]

library(tree)
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)

cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# Best size = 9
pruned.carseats = prune.tree(tree.carseats, best = 9)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)

library(randomForest)

bag.carseats = randomForest(Sales ~ ., data = Carseats.train, 
mtry = 10, ntree = 500,  importance = T)
bag.pred = predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)

importance(bag.carseats)

rf.carseats = randomForest(Sales ~ ., data = Carseats.train,
mtry = 5, ntree = 500, importance = T)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)

importance(rf.carseats)

library(ISLR)
attach(OJ)
set.seed(1013)

train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

library(tree)
oj.tree = tree(Purchase ~ ., data = OJ.train)
summary(oj.tree)

oj.tree

plot(oj.tree)
text(oj.tree, pretty = 0)

oj.pred = predict(oj.tree, OJ.test, type = "class")
table(OJ.test$Purchase, oj.pred)

cv.oj = cv.tree(oj.tree, FUN = prune.tree)

plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")

#Size of 6 gives lowest cross-validation error.

oj.pruned = prune.tree(oj.tree, best = 6)

summary(oj.pruned)

pred.unpruned = predict(oj.tree, OJ.test, type = "class")
misclass.unpruned = sum(OJ.test$Purchase != pred.unpruned)
misclass.unpruned/length(pred.unpruned)

pred.pruned = predict(oj.pruned, OJ.test, type = "class")
misclass.pruned = sum(OJ.test$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)

library(ISLR)
sum(is.na(Hitters$Salary))

Hitters = Hitters[-which(is.na(Hitters$Salary)), ]
sum(is.na(Hitters$Salary))

Hitters$Salary = log(Hitters$Salary)

train = 1:200
Hitters.train = Hitters[train, ]
Hitters.test = Hitters[-train, ]

library(gbm)

set.seed(103)
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
test.errors = rep(NA, length.lambdas)
for (i in 1:length.lambdas) {
    boost.hitters = gbm(Salary~., data = Hitters.train, distribution = "gaussian", 
        n.trees = 1000, shrinkage = lambdas[i])
    train.pred = predict(boost.hitters, Hitters.train, n.trees = 1000)
    test.pred = predict(boost.hitters, Hitters.test, n.trees = 1000)
    train.errors[i] = mean((Hitters.train$Salary - train.pred)^2)
    test.errors[i] = mean((Hitters.test$Salary - test.pred)^2)
}

plot(lambdas, train.errors, type = "b", xlab = "Shrinkage", ylab = "Train MSE", 
    col = "blue", pch = 20)

plot(lambdas,test.errors, type = "b", xlab = "Shrinkage", ylab = "Test MSE", 
    col = "red", pch = 20)

min(test.errors)

lambdas[which.min(test.errors)]

lm.fit = lm(Salary~., data = Hitters.train)
lm.pred = predict(lm.fit, Hitters.test)
mean((Hitters.test$Salary - lm.pred)^2)

library(glmnet)

set.seed(134)
x = model.matrix(Salary~., data = Hitters.train)
y = Hitters.train$Salary
x.test = model.matrix(Salary~., data = Hitters.test)
lasso.fit = glmnet(x, y, alpha = 1)
lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
mean((Hitters.test$Salary - lasso.pred)^2)

#Both the models linear and regularization (Lasso) have 
#higher test MSE than boosting.

boost.best = gbm(Salary~., data = Hitters.train, distribution = "gaussian", 
    n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])
summary(boost.best)

#𝙲𝙰𝚝𝙱𝚊𝚝 CRBI Cwalks are imp variables


library(randomForest)

set.seed(21)
rf.hitters = randomForest(Salary~., data = Hitters.train, ntree = 500, mtry = 19)
rf.pred = predict(rf.hitters, Hitters.test)
mean((Hitters.test$Salary - rf.pred)^2)

#MSE for bagging is 0.23 which less than the boosting
