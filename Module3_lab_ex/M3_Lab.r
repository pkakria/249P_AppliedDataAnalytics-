library(ISLR)
names(Smarket)

dim(Smarket)

summary(Smarket)

#cor(Smarket)
#generating error, because Dir is not numremic

#lag1-lag5- %return of previous trading days
#Volume-No. of shares traded of previous days in billion
#Today- %age return on the date in question
#Direction- whether market was up or dowm
?Smarket

cor(Smarket[, -9])
#produces a matrix that contains all of the pairwise 
#correlations among the predictors in a data set

#observation:correlations between the lag variables and 
#today’s returns are close to zero
#little correlation between today’s returns and previous days’(vol) returns.

attach(Smarket) 
plot(Volume)

#vol is increasing 

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)

# glm() function fits generalized linear models, 
#a class of models that includes logistic regression

summary(glm.fits)

#observations
#lag1 has min p-value, 
#negative coefficient for this predictor suggests that- 
#if the market had a positive return yesterday,
#then it is less likely to go up today

#however, 0.15 is still large value of p
# so there is no clear evidence of a real association 
#between Lag1 and Direction.

#coef() function in order to access the 
#coefficients for this fitted model.
coef(glm.fits)

summary(glm.fits)$coef

summary(glm.fits)$coef[,4]

#The predict() function can be used to predict the probability that 
#the market will go up, given values of the predictors
#The predict() function is used similary to generate predictions
#for the response variable.
glm.probs=predict(glm.fits,type="response")
glm.probs[1:10] #printed only the first 10 probabilities

contrasts (Direction)
#Use the contrasts() function to see the dummy variables generated
#for values in the categorical variable Direction.

#In order to make a prediction as to whether the market will go 
#up or down on a particular day, we must convert these predicted 
#probabilities into class labels, Up or Down. 

glm.pred=rep("Down",1250)  #creates a vector of 1,250 Down elements. 
glm.pred[glm.probs >.5]="Up" #transforms to Up all of the elements exceeds 0.5

#creating a vector of class predictions based on whether
#the predicted probability of a market increase is
#greater than or less than 0.5


#We can generate a confusion matrix between the predicted direction 
#and the actual direction from the variable Direction 
#using the table()function.

table(glm.pred,Direction) #confusion matrix

#diagonal elements of the confusion matrix indicate correct predictions, 
#while the off-diagonals represent incorrect predictions. 
#market would go up on 507 days and that it would go down on 145 days

(507+145) /1250  #correct prediction

#logistic regression correctly predicted the movement of the market 52.2% time
#training error--> 100 − 52.2 = 47.8 % 

#We then divide our dataset into training set and test set. 
#The training set will include observations from 2001-2004 
#and the test set from the year 2005.

train=(Year <2005) #train is a vector of 1,250 elements as observation in data
Smarket.2005= Smarket[! train ,] #pick submatrix of market data, before 2005
dim(Smarket.2005)

Direction.2005=Direction[!train]


glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket,family=binomial,subset=train)

glm.probs=predict(glm.fits,Smarket.2005,type="response")

#we have trained and tested our model on two completely separate-
# training was performed using only the dates before 2005, 
#and testing was performed using only the dates in 2005. 

glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)

#To improve the preditive performance, we can restrict the predictor variables 
#to only those with the strongest relationship to the response variable. 
#In this case, we limit the variables to Lag1 and Lag2.


mean(glm.pred==Direction.2005)

 mean(glm.pred!=Direction.2005)   

glm.fits=glm(Direction~Lag1+Lag2,data=Smarket ,family=binomial, subset=train)

predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),
Lag2=c(1.1,-0.8)),type="response")

library(MASS)

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket ,subset=train)

plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

#The preditc() function for an LDA model returns
#list of three elements representing the predicted class, 
#the posterior probabilities and the linear discriminants
#

lda.class = lda.pred$class
table(lda.class, Direction.2005)

#comparing the predicted class with the predicted directed obtained
#from logistic regression reviously and stored in the vector Direction.2005.

mean(lda.class == Direction.2005)

sum(lda.pred$posterior[, 1] >= 0.5)

sum(lda.pred$posterior[, 1] < 0.5)

lda.pred$posterior[1:20, 1]
#We can inspect the posterior probabilities of the LDA model 
#from the posterior vector of the fitted model.

lda.class[1:20]

#We can also set the posterior probabilities to different
#thresholds for making predictions.

qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

#We can make predictions using predict() just as we did for an LDA model 
#and compare them to the results from the logistic regression
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

mean(qda.class == Direction.2005)

#The class package offers a number of classification algorithms 
#including K-Nearest Neighbors. Before we can run the KNN algorithm, 
#we need to split our dataset into training and test subsets. 
#After splitting the dataset, the cbind() is used to bind the Lag1 and Lag2 variables
#into a matrix for each subset.

library(class)
train.X=cbind(Lag1 ,Lag2)[train ,]  # training data,
test.X=cbind(Lag1,Lag2)[!train,]   # predictors associated with the data on which we make predictions
train.Direction=Direction [train]  #class labels for the training observations

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)

# set.seed() to ensure that repeated runs produce consistent results and then use knn() to 
#make predictions about the market direction in 2005.

(83 + 43)/252

knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)

#can repeat the fit with K = 3.

mean(knn.pred == Direction.2005)

attach(Caravan) #function to make the Caravan dataset available

dim(Caravan)

summary(Purchase)

348/5822

standardized.X <- scale(Caravan[, -86])
var(Caravan[, 1])

#scale() function to scale the dataset with a mean of zero and standard deviation of one.

var(Caravan[, 2])

var(standardized.X[, 1])

var(standardized.X[, 2])

test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)

mean(test.Y != "No")

table(knn.pred, test.Y)

9/(68 + 9)

knn.pred = knn(train.X, test.X, train.Y, k = 3)  #takeing k = 3
table(knn.pred, test.Y)

5/26

knn.pred = knn(train.X, test.X, train.Y, k = 5) #k = 5
table(knn.pred, test.Y)

glm.fit = glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)

#compare the KNN model with a logistic regression using glm() and family = binomial.

glm.probs = predict(glm.fit, Caravan[test, ], type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.5] <- "Yes"
table(glm.pred, test.Y)

glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.25] = " Yes"
table(glm.pred, test.Y)

11/(22 + 11)
