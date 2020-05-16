library("cluster")
require("cluster")
library("fpc")
require("fpc")
library("factoextra")
require("factoextra")
library("ggplot2")
library("gridExtra")
require("gridExtra")
library(cluster)
library(fpc)
library(ggplot2)
library(factoextra)
library(gridExtra)
library("conjoint")

## Loading Data
load("Desktop/Analytics Design/Assignment 3/GBA424 - Toy Horse Case Data.Rdata")

##########################
######### PART A #########
##########################

## Delete NA rows
df1 = conjointData[complete.cases(conjointData$ratings), ]

## Create a new dataframe, which has 5 columns
util = as.data.frame(array(,dim=c(1,5))) 
colnames(util) = c("(intercept)", "pricelow", "26inches", "Rocking", "Glamour")

## Complete individual part-utilities
for (i in 1:200){
   lm1 = lm(ratings~factor(price)+factor(size)+factor(motion)+factor(style), data = df1[df1$ID ==i, ])
   util[i,] = lm1$coefficients
}

############# Prediction ####################
## Produce predictions for missing profiles
fini = conjointData
for (i in (1:3200)){
    if (is.na(conjointData[i,3]) == TRUE){
        fini[i,3] = util[conjointData[i,1],1]+util[conjointData[i,1],2]*conjointData[i,4]+util[conjointData[i,1],3]*conjointData[i,5]+util[conjointData[i,1],4]*conjointData[i,6]+util[conjointData[i,1],5]*conjointData[i,7]
    }
}

############# Ranking ####################
##ranking fini data(the larger number the better)
for (i in 1:200){
    fini$grades = rank(fini[fini$ID == i, ]$ratings)
}

##ranking fini data(the larger number the better)
for (i in (1:200)){
    fini[fini$ID == i, ]$grades = rank(fini[fini$ID == i, ]$ratings)
}

##########################
######### PART B #########
##########################

## Cluster Analysis
clustTest = function(toClust,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
    if(scale){ toClust = scale(toClust);}
    set.seed(seed);   # set random number seed before doing cluster analysis
    wss <- (nrow(toClust)-1)*sum(apply(toClust,2,var))
    for (i in 2:maxClusts) wss[i] <- sum(kmeans(toClust,centers=i,nstart=nstart,iter.max=iter.max)$withinss)
    ##gpw essentially does the following plot using wss above. 
    #plot(1:maxClusts, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
    gpw = fviz_nbclust(toClust,kmeans,method="wss",iter.max=iter.max,nstart=nstart,k.max=maxClusts) #alternative way to get wss elbow chart.
    pm1 = pamk(toClust,scaling=TRUE)
    ## pm1$nc indicates the optimal number of clusters based on 
    ## lowest average silhoutte score (a measure of quality of clustering)
    #alternative way that presents it visually as well.
    gps = fviz_nbclust(toClust,kmeans,method="silhouette",iter.max=iter.max,nstart=nstart,k.max=maxClusts) 
    if(print){
        grid.arrange(gpw,gps, nrow = 1)
    }
    list(wss=wss,pm1=pm1$nc,gpw=gpw,gps=gps)
}

runClusts = function(toClust,nClusts,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
    if(length(nClusts)>4){
        warning("Using only first 4 elements of nClusts.")
    }
    kms=list(); ps=list();
    for(i in 1:length(nClusts)){
        kms[[i]] = kmeans(toClust,nClusts[i],iter.max = iter.max, nstart=nstart)
        ps[[i]] = fviz_cluster(kms[[i]], geom = "point", data = toClust) + ggtitle(paste("k =",nClusts[i]))
        
    }
    library(gridExtra)
    if(print){
        tmp = marrangeGrob(ps, nrow = 2,ncol=2)
        print(tmp)
    }
    list(kms=kms,ps=ps)
}

## Plots a kmeans cluster as three plot report
## pie chart with membership percentages
## ellipse plot that indicates cluster definitions against principle components
## barplot of the cluster means
plotClust = function(km,toClust,discPlot=FALSE){
    nc = length(km$size)
    if(discPlot){par(mfrow=c(2,2))}
    else {par(mfrow=c(3,1))}
    percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
    pie(km$size,labels=percsize,col=1:nc)
    
    clusplot(toClust, km$cluster, color=TRUE, shade=TRUE,
             labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components
    
    if(discPlot){
        plotcluster(toClust, km$cluster,col=km$cluster); #plot against discriminant functions ()
    }
    rng = range(km$centers)
    dist = rng[2]-rng[1]
    locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
    bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
    text(bm,locs,formatC(km$centers,format="f",digits=1))
}


# Choose number of segments
# Choose 3 clusters based on the plot
# nClusts = 3
checks = clustTest(util,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)


##Runs a set of clusters as kmeans
##Arguments:
##  toClust, data.frame with data to cluster
##  nClusts, vector of number of clusters, each run as separate kmeans 
##  ... some additional arguments to be passed to clusters
##Return:
##  list of 
##    kms, kmeans cluster output with length of nClusts
##    ps, list of plots of the clusters against first 2 principle components
clusts = runClusts(util,3,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)

for(i in 1:3) plotClust(clusts[[1]][[i]],util)

# check cluster means
clusts

##########################
######### PART C #########
##########################
df2 = merge(df1, respondentData,by = 'ID')

lmfemale = lm(ratings~factor(price)+factor(size)+factor(motion)+factor(style), data = df2[df2$gender == 1,])
summary(lmfemale)

lmmale = lm(ratings~factor(price)+factor(size)+factor(motion)+factor(style), data = df2[df1$gender ==0, ])
summary(lmmale)

lmage2= lm(ratings~factor(price)+factor(size)+factor(motion)+factor(style), data = df2[df2$age ==0, ])
summary(lmage2)

lmage3 = lm(ratings~factor(price)+factor(size)+factor(motion)+factor(style), data = df2[df2$age ==1, ])
summary(lmage3)

summary(lm(ratings~(factor(price)+factor(size)+factor(motion)+factor(style))*factor(age), data = df2))
summary(lm(ratings~(factor(price)+factor(size)+factor(motion)+factor(style))*factor(gender), data = df2))

fini[fini$grades == 16, ]


##########################
######### PART D #########
##########################

##suppose we choose profile 16&4, competitor is profile 8
senario1 = fini[fini$profile == 16|fini$profile == 4|fini$profile == 8, ]

## ranking senario1 data
for (i in (1:200)){
    senario1[senario1$ID == i, ]$grades = rank(senario1[senario1$ID == i, ]$ratings)
}
senario = senario1[senario1$grades == 3, ]


for (i in (1:200)){
    fini[fini$ID == i, ]$grades = rank(fini[fini$ID == i, ]$ratings)
}

final = fini[fini$grades == 16, ]
marketshare = as.data.frame(table(final$profile))
marketshare$share = marketshare[,2]/200
colnames(marketshare) = c("profile", "number", "share")
marketshare

