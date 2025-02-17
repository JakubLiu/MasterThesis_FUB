library(MASS)
library(matrixcalc)

# this function return a covariance matrix
# input parameters: d (dimensionality), rho (covariance), sigma (variance),
#                   CovStruct ("AR(1)", "AR(2)", "Unstructured")
GenCovMatrix <- function(d,rho,sigma,CovStruct){
  
  if(CovStruct == 'AR(1)'){
    cov_matrix <- matrix(rnorm(d*d, 0, 1), nrow = d, ncol = d)
    for(i in 1:d){
      for(j in 1:d){
        cov_matrix[i,j] <- rho^(abs(i-j))
      }
    }
    cov_matrix <- sigma*cov_matrix
  }
  
  else if(CovStruct == 'AR(2)'){
    cov_matrix <- matrix(rnorm(d*d, 0, 1), nrow = d, ncol = d)
    for(i in 1:d){
      for(j in 1:d){
        cov_matrix[i,j] <- rho^(abs(i-j)/(d-1))
      }
    }
    cov_matrix <- sigma*cov_matrix
  }
  
  else if(CovStruct == 'Toeplitz'){
    cov_matrix <- matrix(rnorm(d*d, 0, 1), nrow = d, ncol = d)
    for(i in 1:d){
      for(j in 1:d){
        cov_matrix[i,j] <- 1 - abs(i-j)/d
      }
    }
    cov_matrix <- sigma*cov_matrix
  }
  
  else if(CovStruct == 'Unstructured'){
    cov_matrix <- matrix(rnorm(d*d, 0, sigma), nrow = d, ncol = d)
    cov_matrix <- cov_matrix %*% t(cov_matrix)
    
    # make matrix symmetric
    for(i in 1:d){
      for(j in 1:d){
        cov_matrix[i,j] <- cov_matrix[j,i]
      }
    }
  }
  
  else{
    message <- paste0("Invalid covariance structure", "\n",
                      "The valid options are: AR(1), AR(2), Toeplitz, Unstructured")
    cat(message)
  }
  
  return(cov_matrix)
  
}


#===========================================================================================================================

# this function returns group means which follow a given alternative pattern
# input parameters d (dimensionality),
#       y = x2_coef*x^2 + x1_coef*x + x0_coef
GenAltPattern <- function(d,x2_coef,x1_coef,x0_coef){
  
  f <- function(x,x2_coef,x1_coef,x0_coef){
    y <- x2_coef*x^2 + x1_coef*x + x0_coef
    return(y)
  }
  
  mean_vec <- 1:d
  for(i in 1:d){
    mean_vec[i] <- f(x = mean_vec[i],
                     x2_coef = x2_coef,
                     x1_coef = x1_coef,
                     x0_coef = x0_coef)
  }
  
  return(mean_vec)
}


#==============================================================================================================================


# this function returns a vector of group means
# input parameters:
#         d --> dimensionality
#         mu0 --> mean under H0
#         mu0_loc --> in which column the mu0 should be located
#         effect_size --> size of the effect
#         loc --> in which columns the effects should reside (example: c(2,5) --> put the effects into the 2nd and 5th columns)
#         effect_direction --> whether to add, subtract or randomly choose one or the other operation on the effect
#             positive --> mu0 + effect
#             negative --> mu0 - effect
#             bidirectional --> mu0 random(+/-) effect
EffectSize <- function(d, mu0, mu0_loc, effect_size, loc, effect_direction = 'bidirectional'){
  
  # sanity checks
  if(d < length(loc)){
    stop("The length of the 'loc' vector can't be longer than the dimensionality of the data")
  }
  if(mu0_loc %in% loc){
    stop("Location of mu0 can't be the same as one of the locations in 'loc'")
  }
  if(effect_direction != 'positive' && effect_direction != 'negative' && effect_direction != 'bidirectional'){
    stop("effect_direction must be one of the following: 'positive', 'negative', 'bidirectional'")
  }
  
  mean_vec <- rep(mu0,d) # a priori populate the mean vector with the H0 mean
  
  # put the effects into their place
  for(i in 1:length(loc)){
    if(effect_direction == 'positive'){sign <- 1}
    else if(effect_direction == 'negative'){sign <- -1}
    else{sign <- round(runif(1,-1,1))}
    mean_vec[loc[i]] <- mu0 + sign*effect_size
  }
  
  return(mean_vec)
}


#==============================================================================================================================



GenerateData <- function(d,n,rho,sigma,CovStruct = 'Unstructured',mu0 = 0,mu0_loc,effect_size,loc,effect_direction = 'bidirectional', mean_vec_manual = NULL){
  
  covariance_matrix <- GenCovMatrix(d=d,
                                    rho=rho,
                                    sigma=sigma,
                                    CovStruct=CovStruct)
  
  if(is.null(mean_vec_manual)){
    mean_vec <- EffectSize(d=d,
                          mu0=mu0,
                          mu0_loc=mu0_loc,
                          effect_size=effect_size,
                          loc=loc,
                          effect_direction=effect_direction)
  }
  else{
    if(length(mean_vec_manual) != d){stop('Length of mean_vec_manual must be equal d')}
    mean_vec <- mean_vec_manual
  }
  
  data <- mvrnorm(n=n,
                  mu=mean_vec,
                  Sigma=covariance_matrix,
                  empirical=FALSE)
  
  list('data' = data, 'CovMatrix' = covariance_matrix, 'mean_vector' = mean_vec)
  
}



# ----------------------------------- TESTING -----------------------------------------

# 
# # example how to generate data with our defined covariance matrix
# covmat <- GenCovMatrix(d=4, rho = 0.5, sigma = 8, CovStruct = 'Unstructured')
# 
# data <- mvrnorm(n = 10, mu = c(1,1,1,1), Sigma = covmat, tol = 1e-6, empirical = FALSE)
# 
# # example how to use the EffectSize function
# mean_vector <- EffectSize(d = 4, mu0 = 0, mu0_loc = 1, effect_size = 10, loc = c(2,4), effect_direction = 'bidirectional')
# 
# 
# 
# hope <- GenerateData(d=4,
#                      n=1000,
#                      rho = 0.5,
#                      sigma=8,
#                      CovStruct = 'AR(2)',
#                      mu0=0,
#                      mu0_loc = 1,
#                      loc = c(2,17),
#                      effect_size = 10,
#                      effect_direction = 'bidirectional',
#                      mean_vec_manual = c(1,2,3,400))
# 
# 
# 
# 
# 
