CoinFlip <- function() {
  #Vector to hold result of each trial
  trialResult <- vector()
  #Vector to hold mean values of all trial
  meanValue <- vector()
  #Create sample outcome of coin flip, find mean and store to appropriate vectors
  for (i in 1:10000) {
    trialResult<-c(trialResult, sample(c(0, 1), size=1, replace=T))
    meanValue<-c(meanValue, mean(trialResult))
  }
  #Plot line graph of trial and mean values
  plot(meanValue, type="l", ylim=c(0,1), xlab = "Trials", ylab="Mean value", col = "red")
}

CLT <- function(type, sampleSize, noOfSamples) {
  #Generate matrix to hold randomly generated values
  sampleValues <- switch (type,
    "normal" = {
      matrix(rnorm(noOfSamples*sampleSize), noOfSamples)
    },
    "uniform" = {
      matrix(runif(noOfSamples*sampleSize), noOfSamples)
    }, {
      print("Sorry method not yet implemented for this distribution.")
      return()
    }
  )
  #Vector to hold mean values of each sample
  sampleMeanValues <- apply(sampleValues, 1, mean) 
  #Prints histogram of one sample
  hist(sampleValues[1,],col="green",main="Distribution of One Sample")
  #Prints histogram of sample means
  hist(sampleMeanValues, col="green", main="Distribution of Sample Means")
  #Plots qqnormal and qqline plots for sample means
  qqnorm(sampleMeanValues, main="QQ normal")
  qqline(sampleMeanValues, col="red")
}



SLR<-function(csvPath) {
  #Reads data from path specified
  dataSet <- read.csv(csvPath)
  print(summary(dataSet))
  
  #plot simple linear regression for sales vs tv
  plot(dataSet$TV, dataSet$Sales, col="red")
  yFitTV = lm(dataSet$Sales~dataSet$TV)
  print(summary(yFitTV))
  abline(yFitTV, col="green")
  
  #plot simple linear regression for sales vs radio
  plot(dataSet$Radio, dataSet$Sales, col="red")
  yFitRadio= lm(dataSet$Sales~dataSet$Radio)
  print(summary(yFitRadio))
  abline(yFitRadio, col="green")
  
  #plot simple linear regression for sales vs newspaper
  plot(dataSet$Newspaper, dataSet$Sales, col="red")
  yFitNewspaper = lm(dataSet$Sales~dataSet$Newspaper)
  print(summary(yFitNewspaper))
  abline(yFitNewspaper, col="green")
}

MLR <- function(csvPath) {
  #Read csv file
  dataSet <- read.csv(csvPath)
  #plot multiple linear regression for sales vs tv+radio+newspaper
  yFit = lm(dataSet$Sales~dataSet$TV+dataSet$Radio+dataSet$Newspaper)
  print(summary(yFit))
}

LogisticRegression <- function(filePath) {
  #Read txt file from provided filePath
  dataSet <- read.delim(filePath)
  #Calculate logistic regression for Y vs (X1+X2+X3)
  yFit = glm(Y~X1+X2+X3, data = dataSet, family = binomial)
  print(yFit)
  print(summary(yFit))
}

LogisticRegressionImproved <- function(filePath) {
  #Read txt file from provided filePath
  dataSet <- read.delim(filePath)
  # Identifies outliers in all of three variables (X1,X2,X3) and stores filtered 
  #value in X1OutFil, X2OutFil, X3OutFil vectors
  X1OutFil <- RemoveOutliers(dataSet$X1)
  X2OutFil <- RemoveOutliers(dataSet$X2)
  X3OutFil <- RemoveOutliers(dataSet$X3)
  #Logistic regression fit for Y vs outliers treated values of (X1, X2, X3)
  yFit = glm(Y~X1OutFil+X2OutFil+X3OutFil, data = dataSet, family = binomial)
  #Print summary of improved logistic regression
  print(summary(yFit))
}

RemoveOutliers <- function(x) {
  #Find first an third quantiles of given data
  qnt <- quantile(x, prob = c(0.25, 0.75))
  #Multiply 1.5 times IQR (inter qunatile range)
  range <- 1.5 * (qnt[2] - qnt[1])
  #Vector to store values after outliers treatment
  outliersTreated <- vector()
  for (value in x){
    #If outlier detected fill value with NA(not available) else store the same value
    if (value > (qnt[2] + range) | value < (qnt[1] - range)) {
        outliersTreated <- c(outliersTreated, NA)
    } else {
        outliersTreated <- c(outliersTreated, value)
    }
  }
  return(outliersTreated)
}
