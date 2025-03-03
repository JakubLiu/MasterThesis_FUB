#!/usr/bin/Rscript

library(foreach)
library(doParallel)

# make cluster for parallel computing
num_cores <- 60  # we will use 60 cores, specify at least num_cores in the slurm script
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# source for data generation functions
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCE_CODE/DataGenerationFunctions.R")

# source for maximum test
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCE_CODE/MaxTest_function.R")

# source for global test
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCE_CODE/GlobalTest_AOV_function.R")


# Define paths to output and log files
output_file_path <- "/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/Sim3_full_parallel_output_table.csv"
log_file_path <- "/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/Sim3_full_parallel.log"

# Variables
covariance_structures <- c('Unstructured', 'AR(1)', 'AR(2)', 'Toeplitz')
sample_sizes <- c(seq(5, 10, 1), seq(11, 100, 10))  # small steps for small values, large steps for large values
effect_sizes <- 1:3
n_iters_maxtest <- seq(1000, 20000, 1000)
n_simul <- length(covariance_structures) * length(sample_sizes) * length(effect_sizes) * length(n_iters_maxtest)

# Constants
effect_placement <- 2
number_of_columns_under_effect <- 1
dimensionality <- 18
correlation <- 0.5
variance <- 8
mean0 <- 0
mean0_location <- 1
alpha <- 0.05
n_rep <- 1000  # number of times the simulation is repeated for each setting



# grid of all possible varianle combinarions
param_grid <- expand.grid(covariance_structures, sample_sizes, effect_sizes, n_iters_maxtest)
colnames(param_grid) <- c('covariance_structures', 'sample_sizes', 'effect_sizes', 'n_iters_maxtest')


# Simulations
simulation_1_table <- foreach(rownum = 1:nrow(param_grid), .combine = rbind, .packages = c("MASS", "matrixcalc", "foreach", "doParallel")) %dopar% {
  
  status <- paste0(round(rownum/n_simul*100,3), "%\n")
  write(status, file = log_file_path, append = TRUE)
  
  
  level0 <- param_grid$covariance_structures[rownum]
  level1 <- param_grid$sample_sizes[rownum]
  level2 <- param_grid$effect_sizes[rownum]
  level3 <- param_grid$n_iters_maxtest[rownum]
  
  # generate covariance matrix for invertibility check
  cov_matrix <- GenCovMatrix(d = dimensionality, rho = correlation, sigma = variance, CovStruct = level0)
  
  # check for inertibility
  if (!is.positive.definite(cov_matrix)) {
    return(data.frame(
      covariance_structures = level0,
      sample_sizes = level1,
      effect_sizes = level2,
      n_iters_maxtest = level3,
      power_global = "Covariance matrix non-invertible",
      power_max = "Covariance matrix non-invertible"
    ))
  }
  
  # If covariance matrix is invertible, repeat the simulations n_rep times
  results <- foreach(i = 1:n_rep, .combine = rbind, packages = c("MASS", "matrixcalc", "foreach", "doParallel")) %dopar% {
    data_obj <- GenerateData(
      d = dimensionality,
      n = level1,
      rho = correlation,
      sigma = variance,
      CovStruct = level0,
      mu0 = mean0,
      mu0_loc = mean0_location,
      effect_size = level2,
      loc = effect_placement,
      effect_direction = 'bidirectional',
      mean_vec_manual = NULL
    )
    
    data <- data_obj$data
    
    global_test_pvalue <- GlobalTest_AOV(data)$p.value
    max_test_pvalue <- MaxTest_small_samples(data, mu0 = mean0, n_iter = level3)
    
    c(global_reject = as.integer(global_test_pvalue <= alpha),
      max_reject = as.integer(max_test_pvalue <= alpha))
  }
  
  # Calculate power
  power_global <- sum(results[, 1]) / n_rep
  power_max <- sum(results[, 2]) / n_rep
  
  return(data.frame(
    covariance_structures = level0,
    sample_sizes = level1,
    effect_sizes = level2,
    n_iters_maxtest = level3,
    power_global = power_global,
    power_max = power_max
  ))
}


write.csv(simulation_1_table, output_file_path, row.names = FALSE)


stopCluster(cl)
