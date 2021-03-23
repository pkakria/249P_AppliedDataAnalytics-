library(ISLR)

set.seed(1)
dsc = scale(USArrests)
a = dist(dsc)^2
b = as.dist(1 - cor(t(dsc)))
summary(b/a)

library(ISLR)
set.seed(2)

hc.complete = hclust(dist(USArrests), method="complete")
plot(hc.complete)

cutree(hc.complete,3)

dsc = scale(USArrests)
hc.s.complete = hclust(dist(dsc), method="complete")
plot(hc.s.complete)

cutree(hc.s.complete,3)

table(cutree(hc.s.complete,3))

table(cutree(hc.s.complete, 3), cutree(hc.complete, 3))

set.seed(5)
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1

pca.out = prcomp(x)
summary(pca.out)


pca.out$x[,1:2]

plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 

km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

km.out = kmeans(x, 2, nstart=20)
km.out$cluster

km.out = kmeans(x, 4, nstart=20)
km.out$cluster

km.out = kmeans(scale(x), 3, nstart=20)
km.out$cluster

km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)


