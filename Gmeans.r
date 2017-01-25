#Q3
#cleanup before start
rm(list=ls(all=T))
library(cluster)
library(mvShapiroTest)

####################################################################
# Implement G-means algorithm                                      #
####################################################################

dataSet <- read.csv("hw45-r3b-test-data.csv")

#This method is repeatedly called from Gmeans to check whehter cluster follow normal distribution or not
RepGmeans <- function(X, centers, alpha){
    #Find k means with given data set
    km = kmeans(X, centers)
    allCenters = c()
    for(i in 1:length(km$size)){
      #Figure out data present in current cluster
      currDataSet = X[km$cluster == i,]
      #Figure out current center
      currCenter = km$centers[i,]
      if (nrow(currDataSet) < 12) {
        next
      }
      #Check for normality of cluster using mvShapiro test
      mvShapiro = mvShapiro.Test(as.matrix(currDataSet))
      #If not normal divide current dataset into 2 clusters else add center to result
      if(mvShapiro$p.value <= alpha){
        #Divide data which is not normal into two clusters
        newCenters <- kmeans(currDataSet, 2)
        allCenters = rbind(allCenters, newCenters$centers[1,], newCenters$centers[2,])
      }
      else{
        allCenters = rbind(allCenters, currCenter)
      }
    }
    return (allCenters)
  }
  
Gmeans = function(X,alpha = 0.0001,k=1){
    #Calculate kmeans with k=1 and add initial centers
    km = kmeans(X, k)
    allCenters = km$centers
    #Run this loop until all clusters are normal
    while(TRUE) {
      #Repeatedly call RepGmeans method
      currentCenters = RepGmeans(X, allCenters, alpha)
      #If no new centers were added then break
      if(length(allCenters) == length(currentCenters)){
        break
      }
      allCenters = currentCenters
    }
    return (kmeans(X, allCenters))
    
}

#Call Gmeans method and store result in km variable
km <- Gmeans(dataSet)

par(mfrow = c(2, 2))
#Plot X1 vs X2
clusplot(dataSet[,c(1, 2)], km$cluster, xlab = "X1", ylab = "X2", main="X1 vs X2", lines=7, col.p = km$cluster+1 )
#Plot X1 vs X3
clusplot(dataSet[,c(1, 3)], km$cluster, xlab = "X1", ylab = "X3", main="X1 vs X3", lines=7, col.p = km$cluster+1 )
#Plot X2 vs X3
clusplot(dataSet[,c(2, 3)], km$cluster, xlab = "X2", ylab = "X3", main="X2 vs X3", lines=7, col.p = km$cluster+1 )
