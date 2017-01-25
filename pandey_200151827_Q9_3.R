normal_random_variable<-rnorm(300, m=24, sd=5)
qqnorm(normal_random_variable, main="Sample normal qqplot of normal random variable with mean 24 and sd 5")
qqline(normal_random_variable)