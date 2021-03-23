#Module_2 Applied
#solution to que 8
library(MASS)
library(ISLR)

install.packages("ISLR")

Auto = read.csv("/Users/priyanka/desktop/Auto.csv", header = T, na.strings = "?")
Auto = na.omit(Auto)
dim(Auto)
summary(Auto)

#8(a)
data(Auto)
lm.fit = lm(mpg~horsepower,data=Auto)
summary(lm.fit)

#8(a)
#(i) yes, there is a relationship which can be calculated by 
#testing null-hypothesis
#F-stat is large and p-value is close to zero, so we reject null-hypothesis. 
#there is a significant relation b/w horsepower and mpg is significant 

#(ii) as the p-value is close to 0, so relationship between 
#predictor and response is strong

#(iii) the relationship b/w mpg and horsepower is negetive. the more horsepower and less 
# mpg fuel efficiency of a vehicle will have.

#(iv) 
predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence", level=0.95) 

predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction", level=0.95)

#8(b)
plot(Auto$horsepower, Auto$mpg)
#abline(lm.fit)
abline(lm.fit,col='red')

#8(c)
par(mfrow=c(2,2))
plot(lm.fit)

# residuals vs fitted value shows the non-linear relation.

#solution to que. 9(a)
pairs(Auto)

#9(b)
cor(subset(Auto, select=-name))

#9(c)
lm.fit_1 = lm(mpg~.-name, data=Auto)
summary(lm.fit_1)

#9 explanation 
#(i):yes there is a relation between predictor and resp. F-stat>1 and
# p-value is smaller

#(ii):from p-values of predictors:disp, weight,year and orgin have significant 
# relation.

#(iii)reg coef for year is 0.7507- means with every year cars are becoming 
#more fuel efficient


#9(d)
par(mfrow=c(2,2))
plot(lm.fit_1)

#residuals are non linear fit
#between 30-35 mpg, there are some high residuals (323,327,328)
#from leverabge plot, 14 has the high leverage 

#9(e)
lm.fit_2 = lm(mpg~cylinders*displacement+displacement*weight, data=Auto)
summary(lm.fit_2)

lm.fit_3 = lm(mpg~displacement+origin+year*weight, data=Auto)
summary(lm.fit_3)

# lm.fit_2,from the p-values we can see the interaction between 
# cylinders and disp not significant, disp-weight has significant interaction.
# lm.fit_3, from year-weight has signif interaction
#interaction b/w cylinders and horsepower is signficant
#horsepower and weight not significant

lm.fit_3 = lm(mpg~cylinders*horsepower+horsepower*weight+displacement*weight+origin+year*weight, data=Auto)
summary(lm.fit_3)

#interactions b/w weight and year 

#9(f)
lm.fit_4 = lm(mpg~cylinders*horsepower+sqrt(horsepower)*sqrt(weight)+displacement*weight+origin+year*weight, data=Auto)
#lm.fit_4 = lm(mpg~log(acceleration)+(horsepower^2)+(year^2)+sqrt(weight)+(origin^2), data=Auto)
summary(lm.fit_4)

#we did no find any significant improvement

#10
data(Carseats)
summary(Carseats)

attach(Carseats)
#attach() fun is used to access the variables present in the data 
#framework without calling the dataframe
library(MASS)
library(ISLR)

?Carseats

#10(a)
lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)

#10(b) explanation
#price- significant - sales droped by 54 for each $1000 increase
#UrbanYes- sales are lower for urban locations - not significant
#USYes- sales are higher in USlocations - significant relation

#10(c) 
#sales= 13.0434-0.5445*Price-0.02191*(UrbanYes)+1.2005*USYes

#10(d)
#Null-hypothisis for predictors- Price and USYes can reject.

#10(e)
lm.fit_n = lm(Sales ~ Price + US)
summary(lm.fit_n)


#10(f)
#In case of lm.fit_n, data fit better than lm.fit. Although values of RMS and
#R-square are almost equal but lm.fit_n has slightly less RMS than
#lm.fit and no of predictors are also less than lm.fit

#10(g) 
#model from 10(e), 95% confidence intervals for the coefficient(s).

confint(lm.fit_n)


#10(h)   //using10(e) model
plot(predict(lm.fit_n), rstudent(lm.fit_n))

#no evidence fo outliners 


#10(h) 
par(mfrow=c(2,2))
plot(lm.fit_n)

#from Leverage plot, few value exceeds appox at 0.07 so this plots show 
#the high leverge observations.

#15
library(MASS)
library(ISLR)

data(Boston)
summary(Boston)

?Boston

names(Boston)

#15(a)
attach(Boston)
lm.zn = lm(crim~zn)
summary(lm.zn)

lm.indus = lm(crim~indus)
summary(lm.indus)
plot(Boston$indus, Boston$crim)
abline(lm.indus, col="red")

lm.chas = lm(crim~chas) 
summary(lm.chas)
plot(Boston$chas, Boston$crim)
abline(lm.chas, col="red")

lm.nox = lm(crim~nox)
summary(lm.nox)
plot(Boston$nox, Boston$crim)
abline(lm.nox, col="red")

lm.rm = lm(crim~rm)
summary(lm.rm)
plot(Boston$rm, Boston$crim)
abline(lm.rm, col="red")

lm.age = lm(crim~age)
summary(lm.age)
plot(Boston$age, Boston$crim)
abline(lm.age, col="red")

lm.dis = lm(crim~dis)
summary(lm.dis)
plot(Boston$dis, Boston$crim)
abline(lm.dis, col="red")

lm.rad = lm(crim~rad)
summary(lm.rad)
plot(Boston$rad, Boston$crim)
abline(lm.rad, col="red")

lm.tax = lm(crim~tax)
summary(lm.tax)
plot(Boston$tax, Boston$crim)
abline(lm.tax, col="red")

lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio)
plot(Boston$ptratio, Boston$crim)
abline(lm.ptratio, col="red")

lm.black = lm(crim~black)
summary(lm.black)
plot(Boston$black, Boston$crim)
abline(lm.black, col="red")

lm.lstat = lm(crim~lstat)
summary(lm.lstat)
plot(Boston$lstat, Boston$crim)
abline(lm.lstat, col="red")

lm.medv = lm(crim~medv)
summary(lm.medv)
plot(Boston$medv, Boston$crim)
abline(lm.medv, col="red")

#Each predictor has statistically significant association 
#with response except chas

#15(b)
lm.all = lm(crim~., data=Boston)
summary(lm.all)

# for zn, dis, rad, black, medv, we can reject null-hyp

#15(c)
    
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]
plot(x, y)
#results vary between single variable and multi-variable linear regression variables. coefficients have reduced
#value in multi-variable regression than single variable regression, due to synergy effect

lm.zn_nl = lm(crim~zn + I(zn^2) + I(zn^3), data=Boston)
summary(lm.zn_nl)

lm.indus_nl = lm(crim~indus + I(indus^2) + I(indus^3), data=Boston)
summary(lm.indus_nl)

lm.chas_nl = lm(crim~chas + I(chas^2) + I(chas^3), data=Boston)
summary(lm.chas_nl)

lm.nox_nl = lm(crim~nox + I(nox^2) + I(nox^3), data=Boston)
summary(lm.nox_nl)

lm.rm_nl = lm(crim~rm + I(rm^2) + I(rm^3), data=Boston)
summary(lm.rm_nl)

lm.age_nl = lm(crim~age + I(age^2) + I(age^3), data=Boston)
summary(lm.age_nl)

lm.dis_nl = lm(crim~dis + I(dis^2) + I(dis^3), data=Boston)
summary(lm.dis_nl)

lm.rad_nl = lm(crim~rad + I(rad^2) + I(rad^3), data=Boston)
summary(lm.rad_nl)

lm.tax_nl = lm(crim~tax + I(tax^2) + I(tax^3), data=Boston)
summary(lm.tax_nl)

lm.ptratio_nl = lm(crim~ptratio + I(ptratio^2) + I(ptratio^3), data=Boston)
summary(lm.ptratio_nl)

lm.lstat_nl = lm(crim~lstat + I(lstat^2) + I(lstat^3), data=Boston)
summary(lm.lstat_nl)

lm.black_nl = lm(crim~black + I(black^2) + I(black^3), data=Boston)
summary(lm.black_nl)

lm.lstat_nl = lm(crim~lstat + I(lstat^2) + I(lstat^3), data=Boston)
summary(lm.lstat_nl)

lm.medv_nl = lm(crim~medv + I(medv^2) + I(medv^3), data=Boston)
summary(lm.medv_nl)

# 15(d) observations:
# medv, ptratio, dis, age, nox, indus have non-linear association with the response crim
