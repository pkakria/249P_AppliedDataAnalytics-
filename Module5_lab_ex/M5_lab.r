library(e1071)

set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x,col=(3-y))

#svm() function can be used to fit a support vector classifier
#when the argument kernel="linear" is used.
dat=data.frame(x=x,y=as.factor(y)) #resp as factor variable
library(e1071)
svmfit=svm(y~., data=dat, kernel="linear", cost=10,
scale=FALSE) #svm the support vector classifier for a given value of the cost parameter.

#False--not to scale each feature to have mean zero or standard deviation one.

plot(svmfit , dat) #plot support vector classifier 

#−1 class is shown in yellow, and +1 class is shown in red, 
#disc boundry is "linear"

svmfit$index

#The support vectors are plotted as crosses and the remaining 
#observations are plotted as circles
#even support vectors are below-

summary(svmfit)

#linear kernel was used with cost=10, and that there were seven 
#support vectors, four in one class and three in the other.

#if we use small value of cost
svmfit=svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit , dat)

svmfit$inde

#smaller cost,larger number of support vectors, coz the margin is now wider.

set.seed (1)
tune.out=tune(svm,y~.,data=dat,kernel="linear", 
ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
##tune() performs ten-fold cross-validation on a set 
summary(tune.out)

#cost=0.1 results in the lowest cross-validation error rate.


bestmod=tune.out$best.model #tune()stores the best model obtained,
summary(bestmod)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

#predict() can be used to predict the class label on a 
#set of test observations, at any given value of the cost parameter. 


#predicting the class labels of these test observations. 
ypred=predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)

svmfit=svm(y~.,data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)

# three additional observation is misclassified.

#In case two classes are linearly separable.
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

#observations are just barely linearly separable. We fit the SV calssification
#and plot the resulting hyperplane, using a very large value of cost 
#so that no observations are misclassified

dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear",cost=1e5)
summary(svmfit)

#No training errors were made and only three support vectors were used.

svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)

#from fig, margin is very narrow as the observations that are not SV, 
#indicated as circles, are very close to the decision boundary. 
#It seems likely that this model will perform poorly on test data.
 #close to the decision boundary. It seems likely that this model will perform poorly on test data.

# trying with smaller value oc cost
svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)

#with cost=1, we misclassify a training observation, but we obtain a 
#much wider margin and make use of sevensupport vectors. 
# this model will perform better on test data than the model with cost=1e5.

set.seed(3)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

plot(x, col=y)

#data randomly split into training and testing groups. 
#fitting the training data using the svm()with a radial kernel and γ = 1

train=sample(200,100)
svmfit=svm(y~., data=dat[train,], kernel="radial", gamma=1,
cost =1)
plot(svmfit,dat[train,])

#rplot shows esulting SVM has a decidedly non-linear boundary.
summary(svmfit)

svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1, cost=1e5)
plot(svmfit ,dat[train ,])

#fair number of training errors in this SVM fit from plot. 
#If we increase the value of cost, we can reduce the n0. of training errors. 
#but there will be more irregular decision boundary that may overfit the data.

set.seed (1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial",
ranges=list(cost=c(0.1,1,10,100,1000),
gamma=c(0.5,1,2,3,4) ))
summary(tune.out)

#the best choice of parameters involves cost=1 and gamma=1

#predict() to view test set predictions for this model 

table(true=dat[-train,"y"], pred=predict(tune.out$best.model, 
                                         newdata=dat[-train ,]))

### # ROC Curves
library(ROCR) 


rocplot=function(pred, truth, ...){
    predob = prediction (pred, truth)
    perf = performance (predob , "tpr", "fpr") 
    plot(perf ,...)
}

svmfit.opt=svm(y~., data=dat[train,], kernel="radial", gamma=2, 
               cost=1,decision.values=T)

fitted=attributes(predict(svmfit.opt,dat[train,],
                          decision.values=TRUE))$decision.values


#Roc plot
par(mfrow=c(1,2))
rocplot(fitted ,dat[train ,"y"],main="Training Data")


#SVM appears to be producing accurate predictions. increasing γ can produce 
#more flexible fit and generate further improvements in accuracy.


### SVM with multiclass
#If the response is a factor containing more than two levels, then the svm()
#function will perform multi-class classification 

set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

#fitting svm to datra
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1) 
plot(svmfit , dat)

#he e1071 library can also be used to perform sv regression, 
#if the resp vector
#passed in to svm() is numerical rather than a factor.

#Application to Gene Expression Data

library(ISLR)
names(Khan)

dim(Khan$xtrain)

dim(Khan$xtest )

length(Khan$ytrain )

length(Khan$ytest)

table(Khan$ytrain)

table(Khan$ytest)

dat=data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain ))
out=svm(y~., data=dat, kernel="linear",cost=10)
summary(out)

table(out$fitted , dat$y)

dat.te=data.frame(x=Khan$xtest , y=as.factor(Khan$ytest ))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

#there are no training errors.because the large number of variables 
#relative to the no. of observations implies that it is easy to find 
#hyperplanes that fully separate the classes. We are most interested not
#in the SV classifier’s performance on the training observations, 
#but rather its performance on the test observations


