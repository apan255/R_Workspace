#Q2
#cleanup before start
rm(list=ls(all=T))
library(pixmap)

files <- list.files("faces-corrected", pattern="*.pgm", full.names=TRUE)
faces <- lapply(files, read.pnm)

#This function extracts gray scale from pnm image
grayScale <- function(image) {
  return (c(image@grey))
}

#This function centralize the data
centerData <- function(x) {
  x-mean(x)
}

#Convert matrix of faces to array
faceData <- as.array(faces)
#Extract grey value for each face
faceData <- lapply(faceData, grayScale)
#create matrix where each column represents 1 face
faceMatrix <- as.matrix(as.data.frame(faceData))
#Centralize the given data
faceMatrix <- t(apply(faceMatrix, 1, centerData))
#Calclate covariance matrix
faceCov <- cov(faceMatrix)
#Calculate eigen vector and values
faceEigens <- eigen(faceCov)
#Convert eigen vectors to unit vectors
normEigen <- normalize.vector(faceEigens$vectors)
#Project faces
recoveredFaces <- faceMatrix %*% normEigen

#Setting window
par(mfrow = c(2, 5))
#Print output of top 10 faces
for (i in 1:10) {
  printFace <- recoveredFaces[,i]
  printFace <- matrix(printFace, nrow=231, ncol=195)
  plot(pixmapGrey(printFace), main=i)
}