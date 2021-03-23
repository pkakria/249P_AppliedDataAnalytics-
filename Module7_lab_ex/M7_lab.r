data(USArrests)

states=row.names(USArrests)  #row of states
states

names(USArrests) #columns of four variables

apply(USArrests,2,mean)  #mean- 2 means col

#on avg. three times as many rapes as murders, and more than eight
#times as many assaults as rapes

apply(USArrests,2,var) #computing variance of four variables

pr.out=prcomp(USArrests,scale=TRUE)
#prcomp()centers the variables to have mean 0, using scale=TRUE
#we scale the variables to have std deviation 1 

names(pr.out)

pr.out$center  #correspond to mean

pr.out$scale  #correspond to sd

pr.out$rotation  
#rotation gives principal component loadings

#4 distict pca which is expected,
#in general min(n − 1, p)informative principal components

dim(pr.out$x)  #matrix x

biplot(pr.out,scale=0)
#with scale=0 arrows are scaled to represent the loadings 

pr.out$rotation=-pr.out$rotation #making mirror image

pr.out$x=-pr.out$x

plot(pr.out,scale=0)

pr.out$sdev   #sd of each pc

pr.var=pr.out$sdev^2  #calculating variance 
pr.var

#To compute the proportion of variance explained by each PC
#we simply divide the variance explained by each PC
#by the total variance explained by all four PC

pve=pr.var/sum(pr.var)
pve

#PC1 explains 62% of the variance in the data, PC2 24.7% and so on.

plot(pve, xlab="Principal Component", ylab="Proportion of variance explained",
    ylim=c(0,1), type="b")

plot(cumsum(pve),xlab="Principal Component", 
     ylab="Proportion of variance explained",
    ylim=c(0,1), type="b") #commulative pve

#cumsum()computes the cumulative sum of the elements of a numeric vector.

#k-means clustering


set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

x

km.out=kmeans(x,2,nstart=20)

km.out

plot(x,col=(km.out$cluster+1),main="k-means clustering results with K=2",
    xlab="",ylab="",pch=20,cex=2)

set.seed (4)
km.out=kmeans(x,3,nstart=20)
km.out

km.out=kmeans(x,2,nstart=20)

km.out$cluster

plot(x, col=(km.out$cluster +1), main="K-Means Clustering Results with K=2",
     xlab="", ylab="", pch=20, cex=2)

set.seed(6)
km.out=kmeans(x,3,nstart=20)
km.out

set.seed(4)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss

km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

hc.complete=hclust(dist(x), method="complete")

hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="",
cex =.9)

plot(hc.average , main="Average Linkage", xlab="", sub="",
cex =.9)

plot(hc.single , main="Single Linkage", xlab="", sub="",
cex =.9)

cutree(hc.complete, 2)

cutree(hc.average, 2)

cutree(hc.single, 2)

cutree(hc.single,4)

xsc=scale(x)

plot(hclust(dist(xsc), method="complete"), main="Hierarchical
Clustering with Scaled Features ")

x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage
with Correlation -Based Distance", xlab="", sub="")

library(ISLR)

nci.labs=NCI60$labs 
nci.data=NCI60$data

dim(nci.data)

nci.labs[1:4]

table(nci.labs)

nci.labs

#PCA on NC160 data
pr.out=prcomp(nci.data, scale=TRUE)

Cols=function(vec){
cols=rainbow(length(unique(vec)))
return(cols[as.numeric(as.factor(vec))]) 
}

par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,
xlab="Z1",ylab="Z2")

plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,
xlab="Z1",ylab="Z3")

summary(pr.out)

plot(pr.out)

pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component",
col =" blue ")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")

#Clustering the Observations of the NCI60 Data
sd.data=scale(nci.data)

par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete
Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs,
main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,
main="Single Linkage", xlab="", sub="",ylab="")

hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)

par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

hc.out

set.seed(4)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters ,hc.clusters)

hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First
Five Score Vectors ")
table(cutree(hc.out,4), nci.labs)


