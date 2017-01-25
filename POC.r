rm(list=ls(all=T))
B=matrix(c(2, 3, 1, 4, 8, 7), nrow=3, ncol=2)
print(B)
centerData <- function(x) {
  print("Hello world")
  print(x)
  return (x-mean(x))
}

yMean <- function(y) {
  print("Hello Happy")
  print(y)
}

A = t(apply(B, 1, centerData))
print(A)
print(nrow(A))
print(ncol(A))
