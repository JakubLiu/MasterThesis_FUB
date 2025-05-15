# just the implementation of a one sample t-test
TTest_OneSample <- function(data, mu0){
  
  N <- length(data)
  numerator <- mean(data) - mu0
  denominator <- sd(data)/sqrt(N)
  T_stat <- numerator/denominator
  return(T_stat)
}



# function for max test without the wild bootstrap approximation
MaxTest_new <- function(data, mu0){
  # ============================================================
  # WHAT IS DONE HERE:
  #   - applying the contrast matrix
  #   - performing the MCTP
  # WHAT IS NOT DONE HERE:
  #   - no centering (in the sense of Z)
  #   - no generation of random weights
  # ===========================================================
  
  # the samples must be the rows
  # the repeated measures/features must be the columns
  n <- nrow(data)
  d <- ncol(data)
  
  # create the contrat matrix
  # this is a comparison to the grand mean contrast matrix (see: https://www.degruyterbrill.com/document/doi/10.1515/ijb-2012-0020/html)
  Id <- matrix(0, nrow = d, ncol = d)
  diag(Id) <- 1
  Jd <- matrix(1, nrow = d, ncol = d)
  Pd <- Id - (1/d)*Jd
  Y <- matrix(0, nrow = n, ncol = d)
  
  # center the data using the contrast matrix
  Y <- data%*%Pd
  
  T0_array <- 1:d  # array to hold the T statistics for all features/columns/repeated measures
  
  # loop over the columns/features/repeated measures
  for(l in 1:d){
    T_l <- TTest_OneSample(data = Y[,l], mu0 = mu0)  # perform a one sample t-test
    T0_array[l] <- abs(T_l) # append the abs(T) statistic to the array
  }
  
  T0_max <- max(T0_array) # calculate the maximum T value for this iteration
  return(T0_max)
}


WildBoot <- function(l){  # l reffers to one column in the data
  W <- sample(c(-1,1), n, replace = TRUE) # generate random signs
  l <- W * l  # multiply the data by the random signs
  T_l <- TTest_OneSample(data = l, mu0 = mu0)  # perform a one sample t-test
  return(abs(T_l))
}



# function for the max test with the wild bootstrap approximation
MaxTest_small_samples_new <- function(data, mu0, n_iter){
  # =====================================================================================
  # WHAT IS DONE HERE:
  #   - centering (in the sense of Z)
  #   - generating of random signs
  #   - applying the contrast matrix
  # =====================================================================================
  
  library(Matrix)
  data <- as.matrix(data)
  
  # the samples must be the rows
  # the repeated measures/features must be the columns
  n <- nrow(data)
  d <- ncol(data)
  
  # create the contrat matrix
  # this is a comparison to the grand mean contrast matrix (see: https://www.degruyterbrill.com/document/doi/10.1515/ijb-2012-0020/html)
  Id <- matrix(0, nrow = d, ncol = d)
  diag(Id) <- 1
  Jd <- matrix(1, nrow = d, ncol = d)
  Pd <- Id - (1/d)*Jd
  Y <- matrix(0, nrow = n, ncol = d)
  
  
  # center the data using the contrast matrix
  Y <- data%*%Pd
  
  # center the variables, i.e. from each column remove the mean of that column
  Z <- apply(Y,2,function(x) x - mean(x))
  
  T0_max_array <- vector(length = n_iter) # array to hold the maximum T values
  
  # here the iterations start (perform a maximum test n_iter times to approximate the distribution of the maximum statistic)
  for(itr in 1:n_iter){
    
    T0_array <- 1:d # array to hold the T statistics for all features/columns/rep.meas.
    
    # loop over the columns/features/repeated measures
    for(l in 1:d){
      
      W <- sample(c(-1,1), n, replace = TRUE) # generate random signs
      Z[,l] <- W * Z[,l]  # multiply the data by the random signs
      T_l <- TTest_OneSample(data = Z[,l], mu0 = mu0)  # perform a one sample t-test    # I THINK WE NEED TO REPLACE THAT BY EQUATION (4) FROM THE PAPER: https://www.degruyterbrill.com/document/doi/10.1515/ijb-2012-0020/html remeber to use abs(Ti)
      T0_array[l] <- abs(T_l) # append the abs(T) statistic to the array
    }
    
    T0_max <- max(T0_array) # calculate the maximum T value for this iteration
    T0_max_array[itr] <- T0_max # append it to the max T array
  }
  
  
  # calculate the pvalue
  T0_max_final <- MaxTest_new(data = data, mu0 = mu0)  # perform MCTP one last time to ...
  T0_max_final <- abs(T0_max_final)
  T0_max_array <- abs(T0_max_array)
  # ... compare its test statistic to our empirical distribution
  pvalue <- length(T0_max_array[T0_max_array > T0_max_final])/length(T0_max_array)
  return(pvalue)
  
}



n <- 10
d <- 18
data <- matrix(rnorm(n*d, 0, 1), nrow = n, ncol = d)


MaxTest_small_samples(data = data, mu0 = 0, n_iter = 100)

#MaxTest(data,0)
