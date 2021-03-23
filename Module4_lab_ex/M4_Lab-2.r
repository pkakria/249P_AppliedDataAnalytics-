library(ISLR)

library(tree)

remove(Carseats)

attach(Carseats)

names(Carseats)

#ifelse() function to create a variable, called High, 
#which takes on a value of Yes if the Sales variable exceeds 8,
#and takes on a value of No otherwise.
#recording sales as binary data from continous data

High=ifelse(Sales<=8,"No","Yes") 
Carseats =data.frame(Carseats ,High)

names(Carseats)

?Carseats

#We can fit a classification tree using tree() function
tree.carseats=tree(High~.-Sales, Carseats)
summary(tree.carseats)

#from summary-training error is 9%, residual mean deviance is 373
plot(tree.carseats)
text(tree.carseats,pretty=0)  #pretty=0 include the catg names for quali predic

tree.carseats

set.seed(4)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
 (89+52)/200

summary(tree.carseats)

set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)

cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev,type="b")
plot(cv.carseats$k, cv.carseats$dev,type="b")

#prune the tree to obtain nine node tree
prune.carseats=prune.misclass(tree.carseats,best=10) 
plot(prune.carseats)
text(prune.carseats,pretty=0)

#to check how well pruned tree perform on test data, we apply predict()
tree.pred=predict(prune.carseats,Carseats.test,type = "class")
table(tree.pred,High.test)
(92+58)/200   

prune.carseats=prune.misclass(tree.carseats,best=7)
plot(prune.carseats)
text(prune.carseats,pretty=0)


tree.pred=predict(prune.carseats,Carseats.test,type = "class")
table(tree.pred,High.test)
(97+49)/200  

#Fitting regression tree
library(MASS)

#creating training set and fit tree to the training data
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type="b")

cv.boston

prune.boston=prune.tree(tree.boston,best=7)
plot(prune.boston)
text(prune.boston,pretty=0)

summary(prune.boston)

yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

yhat.bag=predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

importance(rf.boston)

varImpPlot(rf.boston)

library(gbm)


set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=5000,interaction.depth=4)
summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage =0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
