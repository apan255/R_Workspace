#cleanup before start
rm(list=ls(all=T))

library(ISLR)
attach(Default)

#print(default)

#Q1.a
glm.default <- glm(default~income+balance, data=Default, family="binomial")
summary(glm.default)

#Q1.b
test.error <- function(seed) {
  set.seed(seed)
  #Q1.b.i
  train.set <- sample(nrow(Default), nrow(Default)/5)
  valid.set <- nrow(Default) - train.set
  
  #Q1.b.ii
  glm.train <- glm(default~income+balance, data=Default, family="binomial", subset=train.set)
  summary(glm.train)
  
  #Q1.b.iii
  post.probab <- predict(glm.train, Default[valid.set,], type="response")
  predict <- vector()
  for (i in 1:length(post.probab)) {
    if(post.probab[i] > 0.5)
     predict <- c(predict, "Yes")
    else
      predict <- c(predict, "No")
  }
  
  #Q1.b.iv
  return (mean(predict != Default[valid.set,]$default))
}
#50-50 split into training and validatio set
print(test.error(5))

#Q1.c
print(test.error(6))
print(test.error(7))
print(test.error(8))

#Q1.d
dummy.test.error <- function(seed) {
  set.seed(4)
  train.set <- sample(nrow(Default), nrow(Default)/5)
  valid.set <- nrow(Default) - train.set
  glm.train <- glm(default~income+balance+student, data=Default, family="binomial", subset=train.set)
  post.probab <- predict(glm.train, Default[valid.set,], type="response")
  predict <- vector()
  for (i in 1:length(post.probab)) {
    if(post.probab[i] > 0.5)
      predict <- c(predict, "Yes")
    else
      predict <- c(predict, "No")
  }
  return (mean(predict != Default[valid.set,]$default))
}

print(dummy.test.error())

detach(Default)