library(MASS)
library(ISLR)

attach(Boston) 
#To access the Boston data from the MASS package

head(Boston)

names(Boston)

#simple linear fit
lm.fit = lm(medv~lstat)

lm.fit = lm(medv~lstat, data = Boston)

lm.fit

names(lm.fit)

summary(lm.fit)

coef(lm.fit)

confint(lm.fit)

predict(lm.fit,data.frame(lstat=c(5,10,15)), interval = "confidence")

#predict()used to produce confidence intervals and prediction intervals 
#for the prediction of medv for a given value of lstat.

predict(lm.fit,data.frame(lstat=c(5,10,15)), interval = "prediction")
 

plot(lstat ,medv)
abline(lm.fit)
abline(lm.fit,lwd = 3, col="red")
#abline() draws a line with intercept a and slope b, 
#The lwd=3 command causes the width of the regression line
#to be increased by a factor of 3

plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
#pch used to create different plotting symbols.

plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2)) 
plot(lm.fit)

 plot(predict(lm.fit), residuals(lm.fit))

plot(predict(lm.fit), rstudent(lm.fit))

#residuals and studentized residuals plots can be ploted
#from linear model using residuals() and rstudent()

plot(hatvalues(lm.fit))

which.max(hatvalues (lm.fit))
#identifies the index of the largest element of a vector. it tells
#which observation has the largest leverage statistic.

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

# . can be add all variables as predictors

?summary.lm 

summary(lm.fit)$r.sq
#R^2

summary(lm.fit)$sigma
#RSE 

#library(ISLR)

install.packages("ISLR")

#install.packages("car")


#library(car) 
#vif(lm.fit)

lm.fit1=lm(medv~.-age,data=Boston) 
summary(lm.fit1)

lm.fit1=update(lm.fit, ~.-age)

#Interactions
summary(lm(medv~lstat*age,data=Boston))

#Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

#The near-zero p-value associated with the quadratic term 
#suggests that it leads to an improved model.

#We use the anova() function to further quantify the extent 
#to which the quadratic fit is superior to the linear fit
lm.fit=lm(medv~lstat)
anova(lm.fit ,lm.fit2)

#Model 1 containing only one predictor-lstat
#Model 2 is larger quadratic model that has two 2 predictors:lstat,lstat^2
#The anova() performs a hypothesis test comparing the two models.
#both the models fit the data equally well,
#in model 2, F is 135, and p-value is near to zero, so model 2
#is superior than Model 1

par(mfrow=c(2,2)) > plot(lm.fit2)

# with lstat2 included in the model, there is 
#little noticible pattern in the residuals.

#cubic fit
lm.fit5=lm(medv~poly(lstat,5)) 
summary(lm.fit5)

lm.fit5=lm(medv~poly(lstat,7)) 
summary(lm.fit5)

?lm.fit

#including additional polynomial terms, up to fifth order, 
#leads to an improvement in the model fit! 
#no polynomial terms beyond fifth order have 
#significant p-values in a regression fit.

library(ISLR)


attach(Carseats)

head(Carseats)

names(Carseats)

# Shelveloc is qualitative data, bad-median-good
#R generates dummy variables automatically

lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
 

attach(Carseats)
contrasts (ShelveLoc )

# Shelveloc is qualitative data, bad-median-good
#R generates dummy variables automatically
?contrasts

LoadLibraries = function() {
    library(ISLR)
    library(MASS)
    print("The libraries have been loaded.")
}

LoadLibraries()
