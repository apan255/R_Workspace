#cleanup before start
rm(list=ls(all=T))


####
# Path variables to files
#
####
setwd('.') #Inside code folder - DON'T CHANGE THIS PATH
source('installPackages.R')
source('utils.R')
path_to_data_folder = '../data' # DON'T CHANGE THIS PATH 

#Q1 
CoinFlip()


#Q2
# populationDistribution: string('uniform','normal')
# sampleSize: integer (~30)
# numberOfSamples: integer (>100)
CLT("uniform", 30, 500)

#Q3a
SLR(paste(path_to_data_folder,'/hw23R-Advertising.csv',sep=''))

#Q3b 
MLR(paste(path_to_data_folder,'/hw23R-Advertising.csv',sep=''))

#Q4
LogisticRegression(paste(path_to_data_folder,'/hw23R-q4data.txt',sep=''))

#Q5
LogisticRegressionImproved(paste(path_to_data_folder,'/hw23R-q4data.txt',sep=''))

