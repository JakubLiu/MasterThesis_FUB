simul <- function(n_iter){
  
  
  n_reject <- 0
  pval_array <- 1:n_iter
  
  for(i in 1:n_iter){
    n <- 100
    d <- 18
    X <- data.frame(matrix(
      rnorm(n*d,0,1),
      nrow = n,
      ncol = d
    ))
    
    X[,c(3)] <- rnorm(n,10,1)   # outliers for columns (repreated measurments)
    
    #if covariance_structure == 'unstructured'
    Id <- matrix(rep(0,d*d), nrow = d, ncol = d)
    diag(Id) <- 1
    Jd <- matrix(rep(1,d*d), nrow = d, ncol = d)
    Pd <- Id - (1/d)*Jd
    H <- Pd
    
    T_mat <- t(H)%*%ginv(H%*%t(H))%*%H
    
    Y <- T_mat%*%t(X)
    
    suma <- matrix(rep(0,d*d), nrow = d, ncol = d)
    
    for(k in 1:n){
      suma <- suma + Y[,k]%*%t(Y[,k])
    }
    S_hat <- suma/n
    
    # calculate the A matrix (naive version)_____________________________________________________________
    A <- matrix(rep(0,n*n), nrow = n, ncol = n)
    
    for(k in 1:n){
      for(l in 1:n){
        A[k,l] <- t(Y[,k])%*%Y[,l]
      }
    }
    
    # more optimal version to calculate A
    A_v2 <- apply(Y, 2, function(k) apply(Y, 2, function(l) t(k) %*% l))
    
    
    
    # calculate B0_________________________________________________________________________________________
    B0 <- sum(diag(A))/n  # is this correct?
    
    
    # calculate B1 (naive version)_________________________________________________________________________-
    diag_elems <- diag(A)
    B1 <- 0
    
    for(k in 1:n){
      for(l in 1:n){
        if(k!=l){
          B1 <- B1 + diag_elems[k]*diag_elems[l]
        }
      }
    }
    
    B1 <- B1/(n*(n-1))
    
    # more optimal implementation of B1
    B1_new <- sum(outer(diag_elems, diag_elems, function(k,l) ifelse(k==l, 0, k*l)))/(n*(n-1))
    
    
    
    # calculate B2________________________________________________________________________________________
    A_new <- A
    diag(A_new) <- 0.0
    A_new <- A_new^2
    B2 <- sum(A_new)/(n*(n-1))
    
    Qn <- sum(A)/n
    An <- Qn/B0
    DegFree <- B1/B2
    pval <- pchisq(An, DegFree, lower.tail = FALSE, log.p = FALSE)
    pval_array[i] <- pval
    
    if(pval <= 0.05){
      n_reject <- n_reject + 1
    }
  }
  
  
  list("n_reject" = n_reject, "pvals" = pval_array)
}


sim <- simul(1000)

sim$n_reject
hist(sim$pvals, col = 'blue', breaks = 100)

#___________________________________________________________________________________________________________________

GlobalTest_AOV <- function(data){
  
  n <- nrow(data)
  d <- ncol(data)
  X <- data
  
  
  # calculate contrast matrix__________________________________________________________________________
  #if covariance_structure == 'unstructured'
  Id <- matrix(rep(0,d*d), nrow = d, ncol = d)
  diag(Id) <- 1
  Jd <- matrix(rep(1,d*d), nrow = d, ncol = d)
  Pd <- Id - (1/d)*Jd
  H <- Pd
  
  T_mat <- t(H)%*%ginv(H%*%t(H))%*%H
  
  Y <- T_mat%*%t(X)
  
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
  B1_new <- sum(outer(diag_elems, diag_elems, function(k,l) ifelse(k==l, 0, k*l)))/(n*(n-1))
  
  
  
  # calculate B2________________________________________________________________________________________
  A_new <- A
  diag(A_new) <- 0.0
  A_new <- A_new^2
  B2 <- sum(A_new)/(n*(n-1))
  
  # calculate Qn, An & the degrees of freedom________________________________________________________
  Qn <- sum(A)/n
  An <- Qn/B0
  DegFree <- B1/B2
  
  # calculate the pvalue_____________________________________________________________________________
  pval <- pchisq(An, DegFree, lower.tail = FALSE, log.p = FALSE)
  
  list("test_stat" = An, "p.value" = pval)
}

