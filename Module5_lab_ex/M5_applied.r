set.seed(113)
x = rnorm(100)
y = 3 * x^2 + 4 + rnorm(100)
train = sample(100, 50)
y[train] = y[train] + 3
y[-train] = y[-train] - 3

plot(x[train], y[train], pch="+", lwd=4, col="red", 
     ylim=c(-4, 20), xlab="X", ylab="Y")
points(x[-train], y[-train], pch="o", lwd=4, col="blue")

#plot is non-linearly seperable,
#creating train and test dfs by taking half of +ve and -ve classes 
#to create a new vector

library(e1071)
set.seed(113)
z = rep(0, 100)
z[train] = 1
# Take 25 observations each from train and -train
final.train = c(sample(train, 25), sample(setdiff(1:100, train), 25))
data.train = data.frame(x=x[final.train], y=y[final.train], 
                        z=as.factor(z[final.train]))
data.test = data.frame(x=x[-final.train], y=y[-final.train], 
                       z=as.factor(z[-final.train]))

svm.linear = svm(z~., data=data.train, kernel="linear", cost=10)
plot(svm.linear, data.train)

#plot shows linear boundry

table(z[final.train], predict(svm.linear, data.train))

set.seed(32325)
svm.poly = svm(z~., data=data.train, kernel="polynomial", cost=10)
plot(svm.poly, data.train)

table(z[final.train], predict(svm.poly, data.train))

#This is a default polynomial kernel with degree 3. 

set.seed(353)
svm.radial = svm(z~., data=data.train, kernel="radial", gamma=1, 
                 cost=10)
plot(svm.radial, data.train)

table(z[final.train], predict(svm.radial, data.train))

#this classifier perfectly classifies train data.

plot(svm.linear, data.test)

plot(svm.poly, data.test)

plot(svm.radial, data.test)

table(z[-final.train], predict(svm.linear, data.test))

table(z[-final.train], predict(svm.poly, data.test))

table(z[-final.train], predict(svm.radial, data.test))

#linear, polynomial and radial basis kernels classify
#7, 15, and 1 test points incorrectly respectively. 
#Radial kernel is the best and has less test misclassification error
#than linear and polynomial.

set.seed(333)
x1 = runif(500) - 0.5
x2 = runif(500) - 0.5
y = 1 * (x1^2 - x2^2 > 0)

plot(x1[y == 0], x2[y == 0], col = "red", xlab = "X1",
     ylab = "X2", pch = "+")
points(x1[y == 1], x2[y == 1], col = "blue", pch = 4)

#plot show non-linear decision boundry

lm.fit = glm(y~x1 + x2, family = binomial)
summary(lm.fit)

data = data.frame(x1 = x1, x2 = x2, y = y)
lm.prob = predict(lm.fit, data, type = "response")
lm.pred = ifelse(lm.prob > 0.52, 1, 0)
data.pos = data[lm.pred == 1, ]
data.neg = data[lm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1",
     ylab = "X2", pch = "+")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

#no decision boundry is shown in the plot. all points are are classified in single class

lm.fit = glm(y~poly(x1, 2) + poly(x2, 2) + I(x1 * x2), 
             data = data, family = binomial)

lm.prob = predict(lm.fit, data, type = "response")
lm.pred = ifelse(lm.prob > 0.5, 1, 0)
data.pos = data[lm.pred == 1, ]
data.neg = data[lm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1",
     ylab = "X2", pch = "+")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

#non-linear decision boundary very similar to true decision boundary.

svm.fit = svm(as.factor(y) ~ x1 + x2, data, kernel = "linear",
              cost = 0.1)
svm.pred = predict(svm.fit, data)
data.pos = data[svm.pred == 1, ]
data.neg = data[svm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1", 
     ylab = "X2",
     pch = "+")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

# linear kernel, even with low cost fails to find 
#non-linear decision boundary
#and classifies all points to a single class.

svm.fit = svm(as.factor(y)~x1 + x2, data, gamma = 1)
svm.pred = predict(svm.fit, data)
data.pos = data[svm.pred == 1, ]
data.neg = data[svm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1", 
     ylab = "X2", pch = "+")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

#non-linear decision boundary on predicted labels similar
#true decision boundary.

#from above problem, SVMs of non-linear kernel are extremely powerful 
#in finding non-linear boundary.logistic regression with non-interactions 
#and SVMs with linear kernels fail to find the decision boundary. 
#Adding interaction terms to logistic regression gives same power as 
#radial-basis kernels. However, there is some manual efforts and 
#tuning involved in picking right interaction terms. This effort can become 
#prohibitive with large number of features. Radial basis kernels, 
#on the other hand, only require tuning of one parameter - gamma - 
#which can be easily done using cross-validation.

library(ISLR)
gas.med = median(Auto$mpg)
new.var = ifelse(Auto$mpg > gas.med, 1, 0)
Auto$mpglevel = as.factor(new.var)

library(e1071)

set.seed(3255)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "linear",
                ranges = list(cost = c(0.01, 
    0.1, 1, 5, 10, 100)))
summary(tune.out)

set.seed(21)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial",
                ranges = list(cost = c(0.1, 
    1, 5, 10), degree = c(2, 3, 4)))
summary(tune.out)

set.seed(333)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", 
                ranges = list(cost = c(0.1, 
    1, 5, 10), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)


svm.linear = svm(mpglevel~., data = Auto, kernel = "linear", 
                 cost = 1)
svm.poly = svm(mpglevel~., data = Auto, kernel = "polynomial", 
               cost = 10, 
    degree = 2)
svm.radial = svm(mpglevel~., data = Auto, kernel = "radial", 
                 cost = 10, gamma = 0.01)
plotpairs = function(fit) {
    for (name in names(Auto)[!(names(Auto) 
                               %in% c("mpg", "mpglevel", "name"))]) {
        plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
    }
}
plotpairs(svm.linear)

plotpairs(svm.poly)

plotpairs(svm.radial)


