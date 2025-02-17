TTest_OneSample <- function(data, mu0){
  
  N <- length(data)
  numerator <- mean(data) - mu0
  denominator <- sd(data)/sqrt(N)
  T_stat <- numerator/denominator
  return(T_stat)
}

# function for max test without the wild bootstrap approximation
MaxTest <- function(data, mu0){
  
  # the samples must be the rows
  # the repeated measures/features must be the columns
  n <- nrow(data)
  d <- ncol(data)
  Z <- apply(data,2,function(x) x - mean(x)) # center the variables, i.e. from each column remove the mean of that column
  T0_array <- 1:d  # array to hold the T statistics for all features/columns/rep.meas.
  
  # loop over the columns/features/repeated measures
  for(l in 1:d){
    
    W <- sample(c(-1,1), n, replace = TRUE) # generate random signs
    Z[,l] <- W * Z[,l]  # multiply the data by the random signs
    T_l <- TTest_OneSample(data = Z[,l], mu0 = mu0)  # perform a one sample t-test
    T0_array[l] <- abs(T_l) # append the abs(T) statistic to the array
  }
  
  T0_max <- max(T0_array) # calculate the maximum T value for this iteration
  return(T0_max)
}


# function for the max test with the wild bootstrap approximation
MaxTest_small_samples <- function(data, mu0, n_iter){
  
  library(Matrix)
  
  # the samples must be the rows
  # the repeated measures/features must be the columns
  n <- nrow(data)
  d <- ncol(data)
  
  # ones <- rep(1,n)
  # Pn <- diag(n) - (1/n)*(ones%*%t(ones))
  
  Id <- matrix(rep(0,d*d), nrow = d, ncol = d)
  diag(Id) <- 1
  Jd <- matrix(rep(1,d*d), nrow = d, ncol = d)
  Pd <- Id - (1/d)*Jd
  
  
  Y <- matrix(0, nrow = n, ncol = d)
  Y <- data%*%Pd


  
  # center the variables, i.e. from each column remove the mean of that column
  Z <- apply(Y,2,function(x) x - mean(x))
  
  T0_max_array <- 1:n_iter # array to hold the maximum T values
  
  # here the iterations start
  for(itr in 1:n_iter){
    
    T0_array <- 1:d # array to hold the T statistics for all features/columns/rep.meas.
    
    # loop over the columns/features/repeated measures
    for(l in 1:d){
      
      W <- sample(c(-1,1), n, replace = TRUE) # generate random signs
      Z[,l] <- W * Z[,l]  # multiply the data by the random signs
      T_l <- TTest_OneSample(data = Z[,l], mu0 = mu0)  # perform a one sample t-test
      T0_array[l] <- abs(T_l) # append the abs(T) statistic to the array
    }
    
    T0_max <- max(T0_array) # calculate the maximum T value for this iteration
    T0_max_array[itr] <- T0_max # append it to the max T array
  }
  
  
  # calculate the pvalue
  T0_max_final <- MaxTest(data = data, mu0 = mu0)
  T0_max_final <- abs(T0_max_final)
  T0_max_array <- abs(T0_max_array)
  pvalue <- length(T0_max_array[T0_max_array > T0_max_final])/length(T0_max_array)
  return(pvalue)
  
}



# fix samp size and dim   15,30
# convex, concave, linear

# patterns in the alternative
# example of a linear pattern
# mu0: 1,2,3,4
# mu1: 3,4,5,6
# mu2: 5,6,7,8
# mu3: 7,8,9,10
# and so on...


# n <- 10
# d <- 18
# X <- matrix(rnorm(n*d,0,1), nrow = n, ncol = d)
# mu0 <- 0
# n_simul <- 1000
# n_iter <- 100
# 
# pvals <- 1:n_simul
# 
# for(i in 1:n_simul){
#   status <- paste0(i/n_simul*100, " %")
#   print(status)
#   pval <- MaxTest_small_samples(X,mu0,n_iter)
#   pvals[i] <- pval
# }
# 
# hist(pvals, breaks = 100, col = 'blue')



