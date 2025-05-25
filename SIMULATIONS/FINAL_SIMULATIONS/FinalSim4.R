source('/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/MCTP_WB_superfast.R')
source('/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/DataGenerationFunctions.R')
source('/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/GlobalTest.R')
library(foreach)
library(doParallel)


# simulation outline__________________________________________________________________________________________

# sparse alternative pattern *
#   covariance structure
#     sample size
#       dimensionality
#         effect size

# * The sprase alternative pattern looks like this, that only the last column has an effect (all the others
# havea mean of mu0 (0)).


# make cluster for parallel computing____________________________________________________________________________
num_cores <- 70
cl <- makeCluster(num_cores)
registerDoParallel(cl)


# variables________________________________________________________________________________________________________
var_covariance_structures <- c('AR(1)', 'AR(2)', 'Toeplitz')
var_samplesizes <- c(10,20,30, 40, 50)
var_dimensionalities <- c(10, 20, 30 ,50)
var_effectsizes <- seq(from = 0.01, to = 1.0, length.out = 5)


variable_grid <- expand.grid(var_effectsizes,
                             var_dimensionalities,
                             var_samplesizes,
                             var_covariance_structures)

variable_grid <- variable_grid[,c(4,3,2,1)]

colnames(variable_grid) <- c('var_covariance_structures',
                             'var_samplesizes',
                             'var_dimensionalities',
                             'var_effectsizes')

# constants_____________________________________________________________________________________________________
const_variance <- 1.0
const_correlation <- 0.5
const_mu0 <- 0.0
const_alpha <- 0.05
const_nsim <- 10000
const_nboot <- 10000



# simulate_____________________________________________________________________________________________________
powers_glob <- vector(length = nrow(variable_grid))
powers_mctp <- vector(length = nrow(variable_grid))


# outer loop: loop over all the variable combinations
for(i in 1:nrow(variable_grid)){
  
  # print progress
  status <- paste0(i/nrow(variable_grid)*100, '%')
  print(status)
  
  # fix the variables
  covariance_structure <- variable_grid$var_covariance_structures[i]
  samplesize <- variable_grid$var_samplesizes[i]
  dimensionality <- variable_grid$var_dimensionalities[i]
  effectsize <- variable_grid$var_effectsizes[i]
  
  # generate the mean vector
  means <- rep(0,dimensionality)
  means[length(means)] <- 0 + effectsize  # extreme sparse alternative
  
  # generate the covariance matrix
  cov_mat <- GenCovMatrix(d=dimensionality, rho=const_correlation,
                          sigma=const_variance, CovStruct=covariance_structure)
  
  
  pvalues <- foreach(j=1:const_nsim, .packages = c("MASS", "resample", "matrixStats"), .combine = rbind) %dopar%{
    
    # generate the data
    X <- mvrnorm(n=samplesize, mu=means, Sigma=cov_mat, empirical=FALSE)
    
    # perform the tests and return the pvalues
    pval_glob <- GlobalTest_AOV(X)$p.value
    pval_mctp <- MCTPWB(X,const_mu0,const_nboot)
    c(pval_glob, pval_mctp)
    
  }
  
  # calculate the powers
  powers_glob[i] <- sum(as.integer(pvalues[,1] <= const_alpha))/const_nsim
  powers_mctp[i] <- sum(as.integer(pvalues[,2] <= const_alpha))/const_nsim
}

# stop the cluster___________________________________________________________________________________________________________
stopCluster(cl)

results <- variable_grid
results$powers_glob <- powers_glob
results$powers_mctp <- powers_mctp

write.csv(results, '/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/FINAL_SIMULATIONS/FinalSim4.results.csv')
