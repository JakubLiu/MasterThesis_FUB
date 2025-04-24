library(foreach)
library(doParallel)

# make cluster for parallel computing
num_cores <- 60
cl <- makeCluster(num_cores)
registerDoParallel(cl)


# source for data generation functions
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/DataGenerationFunctions.R")

# source for maximum test
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/MCTP_WB.R")

# source for global test
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/GlobalTest.R")

# source for BradleyLimits
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/BradleyLimits.R")


# Define paths to output and log files___________________________________________________________________________________________
output_file_path <- "/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/Simulation1_results.csv"
log_file_path <- "/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/Simulation1.log"


# variables______________________________________________________________________
covariance_structures <- c('Unstructured', 'AR(1)', 'AR(2)', 'Toeplitz')  # level1
effect_sizes <- 1:3 # level 2

step5_10 <- seq(from = 5, to = 10, by = 1)  # for small sample sizes do small steps
step_11_100 <- seq(from = 11, to = 100, by = 10)   # for large sample sizes to large steps
sample_sizes <- c(step5_10, step_11_100) # level 3

# constants___________________________________________________________________
fixed_effect_placement <- 2
fixed_number_of_columns_under_effect <- 1
fixed_dimensionality <- 18
fixed_correlation <- 0.5
fixed_variance <- 8
fixed_mean0 <- 0
fixed_mean0_location <- 1
fixed_n_iter_maxtest <- 10000
fixed_alpha <- 0.05
# alternative_pattern not relevant for this simulation

n_conditions <- length(covariance_structures) * length(effect_sizes) * length(sample_sizes)  # number of condition combinations
n_simul <- 10000  # number of times the data is simulated under the given conditions


# make a parameter grid based on the variables
param_grid <- expand.grid(sample_sizes, effect_sizes, covariance_structures)
colnames(param_grid) <- c('sample_sizes', 'effect_sizes', 'covariance_structures')





# outer loop over the different simulation parameter values
simulation_table <- foreach(condition = 1:nrow(param_grid), .combine = rbind, .packages = c("MASS", "matrixcalc", "foreach", "doParallel")) %dopar% {
  
  # log the progress (only of the outer loop)
  status <- paste0(round(condition/n_conditions*100,3), "%\n")
  write(status, file = log_file_path, append = TRUE)

  # fix the simulation parameters for the inner loop
  covariance_structure_ <- param_grid$covariance_structures[condition]
  effect_size_ <- param_grid$effect_sizes[condition]
  sample_size_ <- param_grid$sample_sizes[condition]
  
  # generate covariance matrix for invertibility check
  cov_matrix <- GenCovMatrix(d = fixed_dimensionality, rho = fixed_correlation, sigma = fixed_variance, CovStruct = covariance_structure_)
  
  
  # check for invertibility (if it is non-invertible skip the inner loop for that parameter combination)
  if (!is.positive.definite(cov_matrix)) {
    
    
    return(data.frame(
      covariance_structures = covariance_structure_,
      effect_sizes = effect_size_,
      sample_sizes = sample_size_,
      power_global = "Covariance matrix non-invertible",
      power_max = "Covariance matrix non-invertible"
    ))
    
    
  }
  
  
  # if the covariance matrix is invertible, start the simulations for the given parameter setting (the inner loop)
  else{
    
    results <- foreach(i=1:n_simul, .packages = "MASS", .combine = rbind) %dopar%{
      
      # generate the data in each iteration
      data_obj <- GenerateData(d = fixed_dimensionality,
                               n = sample_size_,
                               rho = fixed_correlation,
                               sigma = fixed_variance,
                               CovStruct = covariance_structure_,
                               mu0 = fixed_mean0,
                               mu0_loc = fixed_mean0_location,
                               effect_size = effect_size_,
                               loc = fixed_effect_placement,
                               effect_direction = 'positive',
                               mean_vec_manual = NULL)
      
      data <- data_obj$data # extract only the data itself from the object
      
      
      global_test_pvalue <- GlobalTest_AOV(data)$p.value
      max_test_pvalue <- MaxTest_small_samples(data,mu0=fixed_mean0,n_iter=fixed_n_iter_maxtest)
      
      
      c(global_reject = as.integer(global_test_pvalue <= alpha),
        max_reject = as.integer(max_test_pvalue <= alpha))
      
    }
    
    power.global <- sum(results[, 1])/n_simul
    power.max <- sum(results[, 2])/n_simul
    
    return(data.frame(
      covariance_structures = covariance_structure_,
      effect_sizes = effect_size_,
      sample_sizes = sample_size_,
      power_global = power.global,
      power_max = power.max
    ))
  }
  
}


write.csv(simulation_table, output_file_path)

sink("log_file_path")
B <- BradleyLim(alpha = fixed_alpha, n_simul = n_simul)
out <- paste0("Bradley limits: ", B[1], " ", B[2])
print(out)
sink()

# stop the cluster
stopCluster(cl)


