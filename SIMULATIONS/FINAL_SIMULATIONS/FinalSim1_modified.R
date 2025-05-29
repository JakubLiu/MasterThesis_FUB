# description of simulation_______________________________________________________________________________________

#linear alt alt pattern
#    samplesize
#        dimensionality
#            effectsize


# imports___________________________________________________________________________________________________________
source('/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/MCTP_WB_superfast.R')  # MCTPWB
source('/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/MCTP_WB_superfast_modified.R')  # MCTPWB modified
source('/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/GlobalTest.R')  # Global Test
source('/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/DataGenerationFunctions.R') # for covariance matrx generation
library(foreach)
library(doParallel)


# make cluster for parallel computing____________________________________________________________________________
num_cores <- 70
cl <- makeCluster(num_cores)
registerDoParallel(cl)


# variables________________________________________________________________________________________________________
var_covariance_structures <- c('AR(1)', 'AR(2)', 'Toeplitz')
var_samplesizes <- c(10,20,30, 40, 50)
var_dimensionalities <- c(10, 20, 30 ,50)
var_effectsizes <- c(0.01, 0.03, 0.05, 0.07, 0.1)

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
const_tau <- 0.7



# simulate_____________________________________________________________________________________________________
powers_glob <- vector(length = nrow(variable_grid))
powers_mctp <- vector(length = nrow(variable_grid))
powers_mctp_modified <- vector(length = nrow(variable_grid))

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
  means <- vector(length = dimensionality)
  for(k in 1:dimensionality){ means[k] <- const_mu0 + effectsize*(k-1) }
  
  # generate the covariance matrix
  cov_mat <- GenCovMatrix(d=dimensionality, rho=const_correlation,
                          sigma=const_variance, CovStruct=covariance_structure)
  
  
  pvalues <- foreach(j=1:const_nsim, .packages = c("MASS", "resample", "matrixStats"), .combine = rbind) %dopar%{
    
    # generate the data
    X <- mvrnorm(n=samplesize, mu=means, Sigma=cov_mat, empirical=FALSE)
    
    # perform the tests and return the pvalues
    pval_glob <- GlobalTest_AOV(X)$p.value
    pval_mctp <- MCTPWB(X,const_mu0,const_nboot)
    pval_mctp_modified <- MCTPWB_modified(X,const_mu0,const_nboot,tau=const_tau)
    c(pval_glob, pval_mctp, pval_mctp_modified)
    
  }
  
  # calculate the powers
  powers_glob[i] <- sum(as.integer(pvalues[,1] <= const_alpha))/const_nsim
  powers_mctp[i] <- sum(as.integer(pvalues[,2] <= const_alpha))/const_nsim
  powers_mctp_modified[i] <- sum(as.integer(pvalues[,3] <= const_alpha))/const_nsim
}

# stop the cluster___________________________________________________________________________________________________________
stopCluster(cl)

results <- variable_grid
results$powers_glob <- powers_glob
results$powers_mctp <- powers_mctp
results$powers_mctp_modified <- powers_mctp_modified


write.csv(results, '/media/DANE/home/jliu/MASTER_THESIS_BER/SIMULATIONS/FINAL_SIMULATIONS/FinalSim1_modified.results.csv')
