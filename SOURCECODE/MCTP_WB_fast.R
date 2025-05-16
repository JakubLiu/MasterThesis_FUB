MCTPWB <- function(data,mu0,n_iter){
  
  library(resample)
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
  blocks <- split(ZW, rep(1:n_iter, each = d))  # split ZW into n_iter blocks of size d (the split function returns a flattened vector)
  blocks <- lapply(blocks, function(b) matrix(b, nrow = nrow(ZW))) # reshape the blocks into the correct dimension (n,d)
  
  # function to compute a one smple ttest (mu0 hardcoded to 0) for each of the n_iter blocks of size d
  t_stats_block <- function(block, mu0 = 0) {
    col_means <- colMeans(block)
    col_sds <- colStdevs(block)
    t_stats <- (col_means - mu0) / (col_sds / sqrt(n))
    return(t_stats)
  }
  
  # create the empirical distirbution of the test statistic
  T0_max_array <- sapply(blocks, function(block) max(abs(t_stats_block(as.matrix(block)))))
  
  
  # perform the MCTP
  col_means <- colMeans(Y)
  col_sds <- colStdevs(Y)
  T_max <- max(abs((col_means - 0) / (col_sds / sqrt(n))))
  
  # calculate the pvalue
  pvalue <- mean(T0_max_array > T_max)
  return(pvalue)
}