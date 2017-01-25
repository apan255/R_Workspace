mu=c(69,166)
sigma=matrix(c(9,45,45,400),2,2)
bivariate_norm_rand_var<-mvrnorm(n=100, mu, sigma)
bivariate_norm_rand_var.kde<-kde2d(bivariate_norm_rand_var[,1], bivariate_norm_rand_var[,2], n = 100)
persp(bivariate_norm_rand_var.kde, phi = 50, theta = 50, col="red", border = "green", 
      main="Bivariate Normal Distribution of height and weight", xlab="height", ylab="weight", ticktype="detailed")
