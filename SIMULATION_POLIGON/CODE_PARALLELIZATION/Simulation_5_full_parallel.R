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
output_file_path <- "/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/Sim5_full_parallel_output_table.csv"
log_file_path <- "/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/Sim5_full_parallel.log"


# variables______________________________________________________________________

covariance_structures <- c('Unstructured', 'AR(1)', 'AR(2)', 'Toeplitz')  # level0

step5_10 <- seq(from = 5, to = 10, by = 1)  # for small sample sizes do small steps
step_11_100 <- seq(from = 11, to = 100, by = 10)   # for large sample sizes to large steps
sample_sizes <- c(step5_10, step_11_100) # level 1

dim_step1 <- seq(from = 2, to = 10, by = 1)  # smaller steps for smaller dimensionalities
dim_step2 <- seq(from = 11, to = 100, by = 10)  # larger steps for larger dimensionalities
dimensions <- c(dim_step1, dim_step2)  # level 2

variance <- seq(from = 0, to = 10, by = 1)  # level 3

correlation_abs <- seq(from = 0, to = 1, by = 0.1)  # level 4  (I don't look at the sign of the correlation)



# constants___________________________________________________________________
effect_size_param <- 2
effect_direction_param <- 'bidirectional'
n_iter_maxtest <- 10000
effect_placement <- 2
number_of_columns_under_effect <- 1
mean0 <- 0
mean0_location <- 1
alpha <- 0.05
# alternative_pattern not relevant for this simulation
n_rep <- 1000 # number of times the data is simulated under the given conditions  !!!!!!!!!!!!!!!!
n_simul <- length(covariance_structures) * length(sample_sizes) * length(dimensions) * length(variance) * length(correlation_abs)
n_total_simul <- n_simul*n_rep # number of total simulations taking into account the repetitions
n_variables <- 5
simulation_1_table <- data.frame(matrix(0, nrow = n_simul, ncol = n_variables + 2))
colnames(simulation_1_table) <- c('covariance_structures','sample_sizes', 'dimensions', 'variance', 'correlation_abs', 'power_global', 'power_max')

# grid of all possible varianle combinarions
param_grid <- expand.grid(covariance_structures, sample_sizes, dimensions, variance, correlation_abs)
colnames(param_grid) <- c('covariance_structures', 'sample_sizes', 'dimensions', 'variance', 'correlation_abs')


# Simulations
simulation_1_table <- foreach(rownum = 1:nrow(param_grid), .combine = rbind, .packages = c("MASS", "matrixcalc", "foreach", "doParallel")) %dopar% {
  
  status <- paste0(round(rownum/n_simul*100,3), "%\n")
  write(status, file = log_file_path, append = TRUE)
  
  
  level0 <- param_grid$covariance_structures[rownum]
  level1 <- param_grid$sample_sizes[rownum]
  level2 <- param_grid$dimensions[rownum]
  level3 <- param_grid$variance[rownum]
  level4 <- param_grid$correlation_abs[rownum]
  
  # generate covariance matrix for invertibility check
  cov_matrix <- GenCovMatrix(d = level2, rho = level4, sigma = level3, CovStruct = level0)
  
  # check for invertibility
  if (!is.positive.definite(cov_matrix)) {
    return(data.frame(
      covariance_structures = level0,
      sample_sizes = level1,
      dimensions = level2,
      variance = level3,
      correlation_abs = level4,
      power_global = "Covariance matrix non-invertible",
      power_max = "Covariance matrix non-invertible"
    ))
  }
  
  # If covariance matrix is invertible, repeat the simulations n_rep times
  results <- foreach(i = 1:n_rep, .combine = rbind, packages = c("MASS", "matrixcalc", "foreach", "doParallel")) %dopar% {
    data_obj <- GenerateData(
      d = level2,
      n = level1,
      rho = level4,
      sigma = level3,
      CovStruct = level0,
      mu0 = mean0,
      mu0_loc = mean0_location,
      effect_size = effect_size_param,
      loc = effect_placement,
      effect_direction = 'bidirectional',
      mean_vec_manual = NULL
    )
    
    data <- data_obj$data
    
    global_test_pvalue <- GlobalTest_AOV(data)$p.value
    max_test_pvalue <- MaxTest_small_samples(data, mu0 = mean0, n_iter = n_iter_maxtest)
    
    c(global_reject = as.integer(global_test_pvalue <= alpha),
      max_reject = as.integer(max_test_pvalue <= alpha))
  }
  
  # Calculate power
  power_global <- sum(results[, 1]) / n_rep
  power_max <- sum(results[, 2]) / n_rep
  
  
  #c('covariance_structures', 'sample_sizes', 'dimensions', 'variance', 'correlation_abs')
  
  return(data.frame(
    covariance_structures = level0,
    sample_sizes = level1,
    dimensions = level2,
    variance = level3,
    correlation_abs = level4,
    power_global = power_global,
    power_max = power_max
  ))
}


write.csv(simulation_1_table, output_file_path, row.names = FALSE)


stopCluster(cl)

