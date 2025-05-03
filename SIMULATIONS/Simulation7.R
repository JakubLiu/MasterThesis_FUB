library(foreach)
library(doParallel)

# make cluster for parallel computing
n_cores <- 12
cl <- makeCluster(n_cores)
registerDoParallel(cl)


# source for data generation functions
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/DataGenerationFunctions.R")

# source for (optimized) maximum test
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB_optimized.R")

# source for global test
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/GlobalTest.R")

# source for BradleyLimits
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/BradleyLimits.R")


# Define paths to output and log files___________________________________________________________________________________________
#output_file_path <- "/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/Simulation1_results.csv"
log_file_path <- "C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/log.log"

# variables______________________________________________________________________
covariance_structures <- c('Unstructured', 'AR(1)', 'AR(2)', 'Toeplitz')
correlation_abs <- seq(from = 0.0, to = 1.0, by = 0.1)  # the absolute value of the correlation
sample_sizes <- c(seq(from = 3, to = 10, by = 1), seq(from = 11, to = 100, by = 10))



# constants___________________________________________________________________
fixed_effect_size_param <- 2
fixed_dimensions <- 18
fixed_effect_dir <- 'positive'
fixed_effect_placement <- 2
fixed_number_of_columns_under_effect <- 1
fixed_variance <- 8
fixed_mean0 <- 0
fixed_mean0_location <- 1
fixed_alpha <- 0.05

n_conditions <- length(covariance_structures) * length(correlation_abs) * length(sample_sizes)
n_iter_maxtest <- 2
n_simul <- 2



# make a parameter grid based on the variables
param_grid <- expand.grid(sample_sizes, correlation_abs, covariance_structures)
colnames(param_grid) <- c('sample_sizes', 'correlation_abs', 'covariance_structures')
param_grid <- param_grid[,c('covariance_structures','correlation_abs','sample_sizes')]

# create a placeholder for the output simulation table
simulation_table <- data.frame(matrix(NA, nrow = 1, ncol = 5))
colnames(simulation_table) <- c('covariance_structures', 'correlation_abs', 'sample_sizes', 'power_global', 'power_max')



for(condition in 1:nrow(param_grid)){
  
  # log the progress (only of the outer loop)
  status <- paste0(round(condition/n_conditions*100,3), "%\n")
  write(status, file = log_file_path, append = TRUE)
  
  # fix the simulation parameters for the inner loop
  sample_size_ <- param_grid$sample_sizes[condition]
  correlation_abs_ <- param_grid$correlation_abs[condition]
  covariance_structure_ <- param_grid$covariance_structures[condition]
  
  # first create the covarince matrix to check if it is positive definite
  cov_matrix <- GenCovMatrix(d=fixed_dimensions,
                             rho=correlation_abs_,
                             sigma=fixed_variance,
                             CovStruct=covariance_structure_)
  
  # if the coviariace matrix is not positive definite, then skip to the next iteration
  if(is.positive.definite(cov_matrix) == FALSE){
    
    results_per_condition <- data.frame(
                        covariance_structures = covariance_structure_,
                        correlation_abs = correlation_abs_,
                        sample_sizes = sample_size_,
                        power_global = 'Covarince matrix non-invertible',
                        power_max = 'Covariance matrix non-invertible')
    
    colnames(results_per_condition) <- colnames(simulation_table)
    simulation_table <- data.frame(rbind(simulation_table, results_per_condition))
    next
  }
  
  else{
    
    results <- foreach(i=1:n_simul, .packages = "MASS", .combine = rbind) %dopar%{
      
      data_obj <- GenerateData(d = fixed_dimensions,
                               n = sample_size_,
                               rho = correlation_abs_,
                               sigma = fixed_variance,
                               CovStruct = covariance_structure_,
                               mu0 = fixed_mean0,
                               mu0_loc = fixed_mean0_location,
                               effect_size = fixed_effect_size_param,
                               loc = fixed_effect_placement,
                               effect_direction = fixed_effect_dir,
                               mean_vec_manual = NULL)
      
      data <- data_obj$data # extract only the data itself from the object
      
      
      global_test_pvalue <- GlobalTest_AOV(data)$p.value
      max_test_pvalue <- MaxTest_small_samples_optim(data,mu0=fixed_mean0,n_iter=n_iter_maxtest)
      
      c(global_test_pvalue, max_test_pvalue)
      }
  }
    
    power.global <- sum(as.numeric(results[,1] <= fixed_alpha))/n_simul
    power.max <- sum(as.numeric(results[,2] <= fixed_alpha))/n_simul
    
    results_per_condition <- data.frame(
      covariance_structure_ = covariance_structure_,
      correlation_abs = correlation_abs_,
      sample_sizes = sample_size_,
      power_global = power.global,
      power_max = power.max
    )
    colnames(results_per_condition) <- colnames(simulation_table)
    simulation_table <- data.frame(rbind(simulation_table, results_per_condition))
}
  

simulation_table <- na.omit(simulation_table)


# write.csv(simulation_table, output_file_path)
# 
# sink(log_file_path)
# B <- BradleyLim(alpha = fixed_alpha, n_simul = n_simul)
# out <- paste0("Bradley limits: ", B[1], " ", B[2])
# print(out)
# sink()
 
# stop the cluster
stopCluster(cl)




