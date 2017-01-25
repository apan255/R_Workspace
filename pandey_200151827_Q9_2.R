normal_random_variable<-rnorm(100, m=33, sd=8)
hist(normal_random_variable, freq=F, breaks=seq(from=5, to=60, by=1),
      main="Histogram of normal random variable with mean 33 and sd 8", xlab="Normal Random Variable")