#M1-Applied 
#8(a)
College = read.csv("/Users/priyanka/desktop/College.csv")

dim(College)

#8(b)
head(College)   #pops up the data instead of fix()

#setting row names
rownames(College) = College[,1] 
# droping the first coloumn 
college = College[,-1]  
head(College)

#8(c)i
summary(College)

#8(c)ii
pairs(College[, 1:10])

#8(c)iii
plot(College$Private,College$Outstate)
#attach(College) (alternative way)
#plot(Private,Outstate)

#8(c)iv

Elite = rep("No", nrow(college)) 
#Created a vector with equal length to the columns of of college with 
#the inital value of "No"

Elite[college$Top10perc >50] = "Yes"
#indexing rows of the college data where the Top10perc column 
#is greater than 50 and changing that row value to "Yes"

Elite = as.factor(Elite) 
#converting quanti to quali

college=data.frame(college, Elite)
 #joining the college and the Elite 

summary(college$Elite)   
#78 Elite 

plot(college$Elite, college$Outstate, xlab="Elite", ylab="Outstate")

#8(c)v
par(mfrow=c(3,3))
hist(College$Apps)
hist(college$Accept)
hist(college$Enroll)
hist(college$Top10perc)
hist(college$Top25perc)
hist(college$Books)
hist(college$F.Undergrad)
hist(college$P.Undergrad)
hist(college$Outstate)

#8(c)vi - observations 

par(mfrow=c(1,1))

plot(college$Top10perc, college$Grad.Rate)
#Colleges with the most students from top 10% perc don't necessarily have the 
#highest graduation rate.Also, rate > 100 is wrong

plot(college$Outstate, college$Grad.Rate)
# high tution fee coorelates to high grad rate



#highest accept rate?
acceptance_rate = college$Accept/college$Apps
college[which.min(acceptance_rate), ]
# Princeton univ has the higest acceptance rate

college[which.max(college$Top10perc), ] 
#univ has most 10% of students from high scools - massachusetts

#9
Auto = read.csv("/Users/priyanka/desktop/Auto.csv", header = T, na.strings = "?")
Auto = na.omit(Auto)
dim(Auto)
summary(Auto)

#9(a)

#Quantitative:mpg, cylinders(also qualita), displ, horspower, weight, accel, year
#Qualitative:name,origin

#9(b) range of each quantitative predictor
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)

sapply(Auto[, 1:7], range)

#9(c)
 
sapply(Auto[, 1:7], mean)
sapply(Auto[, 1:7], sd)

#9(d)
new_Auto = Auto [-(10:85), ]
dim(new_Auto) 
new_Auto[9, ] == Auto[9, ]
new_Auto[10, ] == Auto[86, ]

sapply(new_Auto[,1:7],range)
sapply(new_Auto[,1:7],mean)
sapply(new_Auto[,1:7],sd)

#9(e) 
pairs(Auto)


#(e)
par(mfrow=c(2,2))
plot(Auto$mpg, Auto$weight)   #heavier weights correlates with lower mpg
plot(Auto$mpg, Auto$cylinders) #more cylinder and less mpg
plot(Auto$mpg, Auto$year) #cars become more efficient with increasing years
plot(Auto$mpg, Auto$origin) # less correlates 



par(mfrow=c(2,2))
plot(Auto$mpg, Auto$accelration)   
plot(Auto$mpg, Auto$names)
plot(Auto$mpg, Auto$diplacement)
plot(Auto$mpg, Auto$horsepower) 

#obvervations 

# All predictors show some correlation with mpg. 'name' predictor is
#likely to result in overfitting the data and will not generalize well.

#10
#10(a)
library(MASS)  #load in the Boston data set. load library(MASS)
Boston         #calling the object Boston
?Boston        #read about the dataset
dim(Boston)


#data set includes 506 rows and 14 columns
#rows represent observations for each town 
#columns represent features

#10(b)
pairs(Boston)
#from the pairs, observations are-
# crime rate correlates with: age, dis, rad, tax, ptratio
# zn correlates with: indus, nox, age, lstat
# indus with: age, dis
# nox with age, dis
# dis with lstat
# lstat with medv

#10(c)
plot(Boston$age, Boston$crim)
#older homes and more the crime rate

#10(c)
par(mfrow=c(2,2))

plot(Boston$dis, Boston$crim)
#closer to work area, more crime rate

plot(Boston$rad, Boston$crim)
#as the index of accessibility to radial highways is higher and more crime rate

plot(Boston$tax, Boston$crim)
# when tax is high, crime rate is high

plot(Boston$ptratio, Boston$crim)
#higher pupil-teacher ratio by town and high crime rate


#10(d)
par(mfrow=c(1,3))
hist(Boston$crim[Boston$crim>1], breaks=10)
#many cities have low crime rates, but there is more crime rate > 20 which is above 100 (at some 15-18 suburbs)


hist(Boston$tax, breaks=20)
#there is a large divide between suburbs with low tax rates and a peak

hist(Boston$ptratio, breaks=20)
#a few have high ration , but not all high/

#10(e)
dim(subset(Boston, chas == 1))
# 35 suburbs bounds the Charles river

#10(f)
median(Boston$ptratio)
# 19.05

#10(g)
t(subset(Boston, medv == min(Boston$medv)))
summary(Boston)
#crime rate is there, not very best place to live

#10(h)
dim(subset(Boston, rm>7))    #64

dim(subset(Boston, rm>8))    #13

summary(dim(subset(Boston, rm>8)))  
summary(Boston)

# comparitively lower crime (suburbs that average more than eight rooms per dwelling)
