GlobalTest_AOV <- function(data){
  
  library(MASS)
  data <- as.matrix(data)
  n <- nrow(data)
  d <- ncol(data)
  X <- data
  
  
  # calculate contrast matrix (comparisons to the grand mean contrast matrix: https://pubmed.ncbi.nlm.nih.gov/23902695/)
  
  Id <- matrix(0, nrow = d, ncol = d)
  diag(Id) <- 1
  Jd <- matrix(1, nrow = d, ncol = d)
  Pd <- Id - (1/d)*Jd
  
  Y <- Pd%*%t(X)
  
  
  # calculate the estimator of the empirical covariance matrix__________________________________
  suma <- matrix(rep(0,d*d), nrow = d, ncol = d)
  
  for(k in 1:n){
    suma <- suma + Y[,k]%*%t(Y[,k])
  }
  S_hat <- suma/n
  
  # calculate the A matrix_____________________________________________________________________
  A <- apply(Y, 2, function(k) apply(Y, 2, function(l) t(k) %*% l))
  
  
  
  # calculate B0_________________________________________________________________________________________
  B0 <- sum(diag(A))/n
  
  
  # calculate B1 ___________________________________________________________________________________
  diag_elems <- diag(A)
  B1 <- sum(outer(diag_elems, diag_elems, function(k,l) ifelse(k==l, 0, k*l)))/(n*(n-1))
  
  
  
  # calculate B2________________________________________________________________________________________
  A_new <- A
  diag(A_new) <- 0.0
  A_new <- A_new^2
  B2 <- sum(A_new)/(n*(n-1))
  
  # calculate Qn, An & the degrees of freedom________________________________________________________
  Qn <- sum(A)/n
  
  DegFree <- B1/B2 * n/(n-1)
  An <- Qn/B0*DegFree
  
  # calculate the pvalue_____________________________________________________________________________
  pval <- pchisq(An, DegFree, lower.tail = FALSE, log.p = FALSE)
  
  list("test_stat" = An, "p.value" = pval)
}



