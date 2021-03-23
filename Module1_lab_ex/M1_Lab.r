x = c(1,3,2,5)
x

x = c(1, 6, 2)
x

y = c(1, 4, 3)
y

length(x)

length(y)

x + y

#ls():get a list of all objects in the current environment 
#rm()Objects can be removed from the environment with this fucn.
ls()

rm(x, y)
ls()


#To remove all objects from the environment, we first get 
#the list of all objects with the ls() function, and pass this list to the rm() function.
rm(list = ls())

#to get help
?matrix

x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)  #create a matrix
x

#following matrix creation with data in row-order.
matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)

sqrt(x) #sqrt of matrix

x^2 # ^ operator raise each element of the matrix to a power

#The rnorm() can be used to generate random no.s
#rnorm() creates standard normal random variables
#with a mean of 0 and a standard deviation of 1. 
x = rnorm(50)
y = x + rnorm(50, mean = 50, sd = 0.1)
cor(x, y)

#to get  consistent results
set.seed(1303)
rnorm(50)

set.seed(3)
y = rnorm(100)
mean(y)

#mean() sd() var() can be used to calculate the 
#mean,standard dev,variance of a vector

var(y)

sqrt(var(y))

sd(y)

x = rnorm(100)
y = rnorm(100)
plot(x, y)

plot(x, y, xlab = "this is the x-axis", ylab = "this is the y-axis", main = "Plot of X vs Y")

pdf("Figure.pdf")    #save figures
plot(x, y, col = "green")
dev.off()

x = seq(1, 10)  #to generate seq of numbers
x

x = 1:10
x

x = seq(-pi, pi, length = 50)
x

#contour plots
y = x
f = outer(x, y, function(x, y) cos(y)/(1 + x^2))   #outer product
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)   #compute f at every point x,y

contour(x, y, f, nlevels = 15)

fa = (f - t(f))/2   #skew sym matrix
contour(x, y, fa, nlevels = 15)

image(x, y, fa)

persp(x,y,fa)  #produce a three-dimensional plot

persp(x,y,fa,theta=30)

persp(x,y,fa,theta=30,phi=20)

persp(x,y,fa,theta=0,phi=70)

persp(x,y,fa,theta=30,phi=70)

persp(x,y,fa,theta=30,phi=40)

A=matrix(1:16,4,4)
A

A[2,3]

A[c(1,3),c(2,4)]

 A[1:3,2:4]

A [1:2 ,]

A [ ,1:2]

 A[1,]

A[-c(1,3),]

dim(A)

Auto = read.csv("/Users/priyanka/desktop/Auto.csv")

dim(Auto)

head(Auto)

Auto = read.csv("/Users/priyanka/desktop/Auto.csv", header=T,na.strings="?")
Auto=na.omit(Auto)

dim(Auto)

Auto [1:4 ,]

Auto=na.omit(Auto)

dim(Auto)

names(Auto)

plot(Auto$cylinders , Auto$mpg )

attach(Auto)
plot(cylinders,mpg)

cylinders=as.factor(cylinders)
#converts quantitative variables into qualitative variables

?plot

plot(cylinders,mpg, col="red")

plot(cylinders, mpg, col = "red", varwidth = T)

plot(cylinders, mpg, col = "red", varwidth = T, horizontal = T)

plot(cylinders, mpg, col = "red", varwidth = T, xlab = "cylinders", ylab = " MPG ")

hist(mpg)

hist(mpg, col = 2)

hist(mpg, col = 2, breaks = 15)

pairs(Auto)

pairs(~mpg + displacement + horsepower + weight + acceleration, Auto)

plot(horsepower, mpg)
identify(horsepower, mpg, name)
#identify() provides a useful interactive method for identifying the 
#value for a particular variable for points on a plot. 

summary(Auto)

summary(mpg)
