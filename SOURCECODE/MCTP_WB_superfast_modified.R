MCTPWB_modified <- function(data,mu0,n_iter,tau=0.7){
  
  library(resample)
  library(matrixStats)
  n <- nrow(data)
  d <- ncol(data)
  
  # create the contrat matrix
  # this is a comparison to the grand mean contrast matrix (see: https://www.degruyterbrill.com/document/doi/10.1515/ijb-2012-0020/html)
  Id <- matrix(0, nrow = d, ncol = d)
  diag(Id) <- 1
  Jd <- matrix(1, nrow = d, ncol = d)
  Pd <- Id - (1/d)*Jd
  
  
  # center the data using the contrast matrix
  Y <- data%*%Pd
  
  # remove the mean of each column
  Z <- apply(Y,2,function(x) x - mean(x))
  
  W <- matrix(sample(c(-1, 1), n * n_iter, replace = TRUE), nrow = n, ncol = n_iter*d)  # weights for all dimensions across all iterations (n, d*n_iter)
  Z_expanded <- Z[, rep(1:ncol(Z), n_iter)] # just expand Z to it has the same dimensions as W (n, d*n_iter)
  ZW <- Z_expanded * W # multiply the columns by the weights for all iteraions (n, d*n_iter)
  col_means <- colMeans(ZW) # calculate te column means (d*n_iter)
  col_sds <- colStdevs(ZW)  # calculate the column standard deviations (d*n_iter)
  t_stats <- (col_means - mu0) / ((col_sds)^(tau) / sqrt(n))  # calculate the column t-statistics (d*n_iter)
  t_matrix <- matrix(t_stats, nrow = d, ncol = n_iter)  # reshape the t-statistics vector into a matrix of shape (d,n_iter)
  T0_max_array <- colMaxs(abs(t_matrix))  # calculate the maximum test statistic per column
  
  # perform the MCTP
  col_means <- colMeans(Y)
  col_sds <- colStdevs(Y)
  T_max <- max(abs((col_means - 0) / (col_sds / sqrt(n))))
  
  # calculate the pvalue
  pvalue <- mean(T0_max_array > T_max)
  return(pvalue)
}

