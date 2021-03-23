library(ISLR)
summary(Weekly)

summary(Smarket)

names(Weekly)
dim(Weekly)

pairs(Weekly)

#Direction = Weekly$Direction
#Weekly$Direction = NULL
#Weekly$NumericDirection = as.numeric(Direction)  
#Weekly$NumericDirection[Weekly$NumericDirection == 1] = -1  
#Weekly$NumericDirection[Weekly$NumericDirection == 2] = +1 
#Weekly.cor = cor(Weekly)

cor(Weekly[, -9])
#cor() creates matrix that contains all of the pairwise correlations of p. 

#Year and Vol seems to have a relationship

#attach(Weekly)
#plot(Volume)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial)
coef(glm.fits)
summary(glm.fits)
#Lag 2 appears to have some statistical significance with a Pr(>|z|) = 3% appx

dim(Weekly)

summary(Weekly)

dim(Weekly$Direction)

glm.probs=predict(glm.fits,type="response")
glm.pred=ifelse(glm.probs>.5,"Up","Down")
table(glm.pred,Weekly$Direction)


#Logistic regression using only Lag2 as the predictor 
#(since it is the most significant predictor)

train.year <- Weekly$Year %in% (1990:2008)
train = Weekly[train.year,]
test = Weekly[!train.year,]
fit2 = glm(Direction~Lag2, data=train, family=binomial)
fit2.prob = predict(fit2, test, type="response")
fit2.pred <- ifelse(fit2.prob > 0.5, "Up", "Down")
table(fit2.pred, test$Direction)


mean(fit2.pred == test$Direction)


fit.lda = lda(Direction~Lag2, data=train)
fit.lda.pred = predict(fit.lda, test)$class
table(fit.lda.pred, test$Direction)

mean(fit.lda.pred == test$Direction)
#accuracy is 62%

fit.qda = qda(Direction~Lag2, data=train)
fit.qda.pred = predict(fit.qda, test)$class
table(fit.qda.pred, test$Direction)
mean(fit.qda.pred == test$Direction)

#accuracy 58%

#require(class)
library(class)
set.seed(1)
train.X = as.matrix(train$Lag2)
test.X = as.matrix(test$Lag2)
knn.pred = knn(train.X, test.X, train$Direction, k=1)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)  

#accuracy 0.5

### # 10(h)
#Logistic Regression and LDA produced the best results

knn.pred = knn(train.X, test.X, train$Direction, k=5)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)
knn.pred = knn(train.X, test.X, train$Direction, k=15)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)
knn.pred = knn(train.X, test.X, train$Direction, k=30)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)
knn.pred = knn(train.X, test.X, train$Direction, k=50)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)

#higher value of K gives best results, predictor-lag2

library(MASS)
summary(Boston)

attach(Boston)
crime01 = rep(0, length(crim))
crime01[crim > median(crim)] = 1
Boston = data.frame(Boston, crime01)

train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crime01.test = crime01[test]

# logistic regression
glm.fit = glm(crime01 ~ . - crime01 - crim, data = Boston, family = binomial, 
    subset = train)

glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crime01.test)

#18.2% test error rate(appx)

glm.fit = glm(crime01 ~ . - crime01 - crim - chas - tax, data = Boston, family = binomial, 
    subset = train)

glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crime01.test)

#18.6% test error rate

# LDA
lda.fit = lda(crime01 ~ . - crime01 - crim, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

#13.4% test error rate

lda.fit = lda(crime01 ~ . - crime01 - crim - chas - tax, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

#12.3%test error rate

lda.fit = lda(crime01 ~ . - crime01 - crim - chas - tax - lstat - indus - age, 
    data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

#11.9% test error rate

# KNN
library(class)
train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
    lstat, medv)[train, ]
test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
    lstat, medv)[test, ]
train.crime01 = crime01[train]
set.seed(1)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.crime01, k = 1)
mean(knn.pred != crime01.test)

#45.8% test error rate 

# KNN(k=10)
knn.pred = knn(train.X, test.X, train.crime01, k = 10)
mean(knn.pred != crime01.test)

#11.1% test error rate 

# KNN(k=100)
knn.pred = knn(train.X, test.X, train.crime01, k = 100)
mean(knn.pred != crime01.test)

#49% test error rate

# KNN(k=10) with subset of variables
train.X = cbind(zn, nox, rm, dis, rad, ptratio, black, medv)[train, ]
test.X = cbind(zn, nox, rm, dis, rad, ptratio, black, medv)[test, ]
knn.pred = knn(train.X, test.X, train.crime01, k = 10)
mean(knn.pred != crime01.test)

#27.8% test error rate
