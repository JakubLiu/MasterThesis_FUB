# just the implementation of a one sample t-test
TTest_OneSample <- function(data, mu0){
  
  N <- length(data)
  numerator <- mean(data) - mu0
  denominator <- sd(data)/sqrt(N)
  T_stat <- numerator/denominator
  return(T_stat)
}



# function for max test without the wild bootstrap approximation
MaxTest_optim <- function(data, mu0){
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
  Y <- Pd%*%t(data)
  Y <- t(Y)   # IS THIS OKAY TO JUST TRANSPOSE IT? IF I DON'T TRANSPOSE IT I GET A DIMENSIONS ERROR
  
  # loop over the columns/features/repeated measures
  T0_array <- apply(Y, MARGIN = 2, function(l) abs(TTest_OneSample(data = l, mu0 = mu0)))
  
  T0_max <- max(T0_array) # calculate the maximum T value for this iteration
  return(T0_max)
}


WildBoot <- function(l, mu0){  # l reffers to one column in the data
  n <- length(l)
  W <- sample(c(-1,1), n, replace = TRUE) # generate random signs
  l <- W * l  # multiply the data by the random signs
  T_l <- TTest_OneSample(data = l, mu0 = mu0)  # perform a one sample t-test
  return(abs(T_l))
}




# function for the max test with the wild bootstrap approximation
MaxTest_small_samples_optim <- function(data, mu0, n_iter){
  # =====================================================================================
  # WHAT IS DONE HERE:
  #   - centering (in the sense of Z)
  #   - generating of random signs
  # WHAT IS NOT DONE HERE:
  #   - applying the contrast matrix (this is done in the MCTP without the WB resampling)
  # =====================================================================================
  
  library(Matrix)
  library(foreach)
  data <- as.matrix(data)
  
  # the samples must be the rows
  # the repeated measures/features must be the columns
  n <- nrow(data)
  d <- ncol(data)
  
  # center the variables, i.e. from each column remove the mean of that column
  Z <- apply(data,2,function(x) x - mean(x))
  
  # here the iterations start (perform a maximum test n_iter times to approximate the distribution of the maximum statistic)
  T0_max_array <- foreach(itr = 1:n_iter, .packages = "MASS", .combine = rbind, .export = c("WildBoot", "n", "TTest_OneSample", "mu0")) %dopar%{
    T0_array <- apply(Z, MARGIN = 2, FUN = WildBoot, mu0 = mu0) #  loop over the columns/features/repeated measures (each iteration is a standalone maximum test)
    T0_max <- max(T0_array) # calculate the maximum T value for this iteration
    c(T0_max)
  }
  
  
  # calculate the pvalue
  T0_max_final <- MaxTest(data = data, mu0 = mu0)
  T0_max_final <- abs(T0_max_final)
  T0_max_array <- abs(T0_max_array)
  pvalue <- length(T0_max_array[T0_max_array > T0_max_final])/length(T0_max_array)
  return(pvalue)
  
}

