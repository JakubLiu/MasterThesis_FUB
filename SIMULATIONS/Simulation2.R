library(foreach)
library(doParallel)

# make cluster for parallel computing
n_cores <- 80
cl <- makeCluster(n_cores)
registerDoParallel(cl)


# source for data generation functions
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/DataGenerationFunctions.R")

# source for (optimized) maximum test
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/MCTP_WB_optimized.R")

# source for global test
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/GlobalTest.R")

# source for BradleyLimits
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/BradleyLimits.R")


# Define paths to output and log files___________________________________________________________________________________________
output_file_path <- "/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/Simulation2_results.csv"
log_file_path <- "/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/Simulation12.log"


# variables______________________________________________________________________
# linear ==> y = 0.1x + 0.1
# convex ==> y = 0.1x^2 + 0.1x + 0.1
# concave ==> y = (-0.1)x^2 + 0.1x + 0.1
alternative_patterns <- list(c(0,0.1,0.1, 1), c(0.1,0.1,0.1, 2), c(-0.1,0.1,0.1, 3))
sample_sizes <- seq(from = 5, to = 100, by = 2)

# constants___________________________________________________________________
fixed_dimensionality <- 30   # the dimensionality can't be to low, because otheriwse the alternative pattern will not show
fixed_variance <- 8
fixed_mean0_location <- 1  # this simulation is different in this sense that we do not specify mu0 = 0, but we specify the column which represents mu0
fixed_n_iter_maxtest <- 1000
fixed_alpha <- 0.05


n_conditions <- length(alternative_patterns) * length(sample_sizes)
n_simul <- 1000  # number of times the data is simulated under the given conditions

# make a parameter grid based on the variables
param_grid <- expand.grid(sample_sizes, alternative_patterns)
colnames(param_grid) <- c('sample_sizes', 'alternative_patterns')

# create a placeholder for the output simulation table
simulation_table <- data.frame(matrix(NA, nrow = 1, ncol = 4))
colnames(simulation_table) <- c('alterative_patterns', 'sample_sizes', 'power_global', 'power_max')



for(condition in 1:nrow(param_grid)){
  
  # log the progress (only of the outer loop)
  status <- paste0(round(condition/n_conditions*100,3), "%\n")
  write(status, file = log_file_path, append = TRUE)
  
  # fix the simulation parameters for the inner loop
  sample_size_ <- param_grid$sample_sizes[condition]
  alternative_pattern_ <- param_grid$alternative_patterns[condition]
  
  if(unlist(alternative_pattern_)[4] == 1){
    alternative_pattern_name <- 'linear'
  }
  else if(unlist(alternative_pattern_)[4] == 2){
    alternative_pattern_name <- 'convex'
  }
  else{
    alternative_pattern_name <- 'concave'
  }
  
  # for the given parameter combination start the simulations
  results <- foreach(i=1:n_simul, .packages = "MASS", .combine = rbind) %dopar%{
    
    group_means <- GenAltPattern(d = fixed_dimensionality,
                                 x2_coef = unlist(alternative_pattern_)[1],
                                 x1_coef = unlist(alternative_pattern_)[2],
                                 x0_coef = unlist(alternative_pattern_)[3])
    
    data <- gen_data_given_grp_means(grp_means = group_means,
                                     n = sample_size_,
                                     var = fixed_variance)
    
    
    global_test_pvalue <- GlobalTest_AOV(data)$p.value
    max_test_pvalue <- MaxTest_small_samples_optim(data,mu0=mean(data[,fixed_mean0_location]),n_iter=fixed_n_iter_maxtest)
    
    c(global_test_pvalue, max_test_pvalue)
    
  }
  
  power.global <- sum(as.numeric(results[,1] <= fixed_alpha))/n_simul
  power.max <- sum(as.numeric(results[,2] <= fixed_alpha))/n_simul
  
  results_per_condition <- data.frame(
    alternative_patterns = alternative_pattern_name,
    sample_sizes = sample_size_,
    power_global = power.global,
    power_max = power.max
  )
  colnames(results_per_condition) <- colnames(simulation_table)
  simulation_table <- data.frame(rbind(simulation_table, results_per_condition))
}
  

simulation_table <- na.omit(simulation_table)


write.csv(simulation_table, output_file_path)
 
sink(log_file_path)
B <- BradleyLim(alpha = fixed_alpha, n_simul = n_simul)
out <- paste0("Bradley limits: ", B[1], " ", B[2])
print(out)
sink()
 
# stop the cluster
stopCluster(cl)



