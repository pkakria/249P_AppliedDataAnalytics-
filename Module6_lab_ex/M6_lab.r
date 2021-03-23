library(ISLR)

set.seed(2)
train=sample(392,196)  #to split teh set of observations

#?sample

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
#subset option in lm() to fit a linear regression using only
#observations corresponding to the training set.

attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#predict() to estimate the resp of all 392 observations
#Mean() to find MSE of 196 observations
#-train selects observations are not in traing set

#estimated test MSE is 25.72. 

#poly() to estimate test error for quad and cubic regression
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train) 
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower ,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#error rates are 20.4 and 20.3

set.seed(1)
train=sample(392,196)
lmfit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower ,2),data=Auto,subset=train) 
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower ,3),data=Auto,subset=train) 
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#if we choose diff training set, we obtain diff errors
#error rates with lm,quad, cubic 22.44,18.71,18.79

library(boot)

glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
#cv.glm() produces a list of components 

#Our cross-validation estimate for the test error
#is approximately 24.23. 

#we can automate this process using for loop
cv.error=rep(0,5)
for (i in 1:5){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

#sharp drop in estimated test MSE b/w linear to quad but not much improvment
#in further higher order polynomials 

set.seed(17)
cv.error.10=rep(0,10)

for (i in 1:10){
glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}

cv.error.10

#computation time is much shorter than LOOCV.
# higher-order polynomial terms leads to lower test error than 
#simply using a quadratic fit.

alpha.fn=function(data,index){
X=data$X[index]
Y=data$Y[index]
return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))) 
}
#the alpha fn returns an estimating alpha to the obvervations imdexed by index.

alpha.fn(Portfolio,1:100) #estimating alpha using all 100 observations


#recording all of the corresponding estimates for α, and computing sd,
#to make this process automate, we use boot()
boot() 
boot(Portfolio ,alpha.fn,R=1000)

#The final output shows that using the original data, αˆ = 0.5758, and that the bootstrap estimate
#for SE(αˆ) is 0.0905.

#fn takes in the Auto data set as well as a set of indices for the observations, 
#and returns the intercept and slope estimates for the linear regression model

boot.fn=function(data,index)
return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)

#alternate way to use boot.fn to create bootstrap estimate
set.seed (1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

boot(Auto ,boot.fn ,1000)

#This indicates that the bootstrap estimate for SE(β0) is 0.84, and 
#the bootstrap estimate for SE(β1) is 0.0074

summary(lm(mpg~horsepower ,data=Auto))$coef

#standard error estimates for β0 and β1 obtained are 
#0.717 for the intercept and 0.0064 for the slope.

boot.fn=function(data,index)
+coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,
subset=index))
set.seed (1)
boot(Auto,boot.fn,1000)
