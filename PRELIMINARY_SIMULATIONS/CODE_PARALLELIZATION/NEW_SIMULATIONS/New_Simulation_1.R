library(foreach)
library(doParallel)

# make cluster for parallel computing
cores <- detectCores()
cl <- makeCluster(floor(cores[1]/2))
registerDoParallel(cl)


# source for data generation functions
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/SIMULATION_POLIGON/DataGenerationFunctions.R")

# source for maximum test
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MAX_TEST/MaxTest_function.R")

# source for global test
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/GLOBAL_TEST_ANOVA/GlobalTest_AOV_function.R")




# output and log file paths
log_file_path = "C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/SIMULATION_POLIGON/NEW_SIMULATIONS/please.log"

# ====================================================== SIMULATION 1 ============================================================

# constants_____________________________________________________________________
covariance_structure <- 'Unstructured'
effect_size_param <- 3
effect_direction_param <- 'positive'
dimensionality <- 18
correlation <- 0.5
variance <- 8
mean0 <- 0
mean0_location <- 1
n_iter_maxtest <- 10
alpha <- 0.05
n_rep <- 10


# variables______________________________________________________________________
sample_sizes <- c(10,30,50,70,90,110,130)
number_of_columns_under_effect <- 1:(dimensionality-1)  # one column is always reserved for the mu0


# other simulation parameters_______________________________________________________
n_simul <- length(sample_sizes) * length(number_of_columns_under_effect)
n_variables <- 2
simulation_table <- data.frame(matrix(0, nrow = n_simul, ncol = n_variables + 2))
colnames(simulation_table) <- c('sample_sizes', 'num_cols_under_effect', 'power_global', 'power_max')



# grid of all possible varianle combinarions
param_grid <- expand.grid(sample_sizes, number_of_columns_under_effect)
colnames(param_grid) <- c('sample_sizes', 'num_cols_under_effect')



# Simulations
simulation_table <- foreach(rownum = 1:nrow(param_grid), .combine = rbind, .packages = c("MASS", "matrixcalc", "foreach", "doParallel")) %dopar% {
  
  status <- paste0(round(rownum/n_simul*100,3), "%\n")
  write(status, file = log_file_path, append = TRUE)
  
  
  level1 <- param_grid$sample_sizes[rownum]
  level2 <- param_grid$num_cols_under_effect[rownum]
  
  # generate covariance matrix for invertibility check
  cov_matrix <- GenCovMatrix(d = dimensionality, rho = correlation, sigma = variance, CovStruct = covariance_structure)
  
  write(is.positive.definite(cov_matrix), file = log_file_path, append = TRUE)
  
  # check for invertibility
  if (!is.positive.definite(cov_matrix)) {
    return(data.frame(
      sample_sizes = level1,
      num_cols_under_effect = level2,
      power_global = "Covariance matrix non-invertible",
      power_max = "Covariance matrix non-invertible"
    ))
  }
  
  # If covariance matrix is invertible, repeat the simulations n_rep times
  results <- foreach(i = 1:n_rep, .combine = rbind, packages = c("MASS", "matrixcalc", "foreach", "doParallel")) %dopar% {
    
    effect_loc <- sample(2:dimensionality, size = level2, replace = FALSE)  # in how many and which columns the effect should be
    print(c(level2, effect_loc))
    
    
    data_obj <- GenerateData(
      d = dimensionality,
      n = level1,
      rho = correlation,
      sigma = variance,
      CovStruct = covariance_structure,
      mu0 = mean0,
      mu0_loc = mean0_location,
      effect_size = effect_size_param,
      loc = effect_loc,
      effect_direction = effect_direction_param,
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
    sample_sizes = level1,
    num_cols_under_effect = level2,
    power_global = power_global,
    power_max = power_max
  ))
}


View(simulation_table)

#write.csv(simulation_1_table, output_file_path, row.names = FALSE)


stopCluster(cl)
