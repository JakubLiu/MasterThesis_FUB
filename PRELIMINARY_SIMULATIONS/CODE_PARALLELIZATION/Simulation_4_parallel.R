#!/usr/bin/Rscript

library(foreach)
library(doParallel)

# make cluster for parallel computing
num_cores <- 60  # we will use 100 cores, specify at least num_cores in the slurm script
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# source for data generation functions
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCE_CODE/DataGenerationFunctions.R")

# source for maximum test
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCE_CODE/MaxTest_function.R")

# source for global test
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCE_CODE/GlobalTest_AOV_function.R")


#==================================================================================================================



# variables______________________________________________________________________

covariance_structures <- c('Unstructured', 'AR(1)', 'AR(2)', 'Toeplitz')  # level 1

dim_step1 <- seq(from = 2, to = 10, by = 1)  # smaller steps for smaller dimensionalities
dim_step2 <- seq(from = 11, to = 100, by = 10)  # larger steps for larger dimensionalities
dimensions <- c(dim_step1, dim_step2)  # level 2



# constants___________________________________________________________________
effect_size_param <- 1
effect_dir <- 'bidirectional'
effect_placement <- 2
number_of_columns_under_effect <- 1
sample_size <- 10
correlation <- 0.5
variance <- 8
mean0 <- 0
mean0_location <- 1
alpha <- 0.05
n_iter_maxtest <- 10000
# alternative_pattern not relevant for this simulation
n_rep <- 1000  # number of times the data is simulated under the given conditions
n_simul <- length(covariance_structures) * length(dimensions)
n_total_simul <- n_simul*n_rep # number of total simulations taking into account the repetitions
n_variables <- 2
simulation_1_table <- data.frame(matrix(0, nrow = n_simul, ncol = n_variables + 2))
colnames(simulation_1_table) <- c('covariance_structures','dimensions', 'power_global', 'power_max')


# ================================================================================================================


rownum <- 1
for(level1 in covariance_structures){
  for(level2 in dimensions){
        
        status <- paste0(round(rownum/n_simul*100,3), '%')
        print(status)
        simulation_1_table$covariance_structures[rownum] <- level1
        simulation_1_table$dimensions[rownum] <- level2
        
        # first create the covarince matrix to check if it is positive definite
        cov_matrix <- GenCovMatrix(d=level2,
                                   rho=correlation,
                                   sigma=variance,
                                   CovStruct=level1)
        
        # if the coviariace matrix is not positive definite, then skip to the next iteration
        if(is.positive.definite(cov_matrix) == FALSE){
          simulation_1_table$power_global[rownum] <- 'Covariance matrix non-invertible'
          simulation_1_table$power_max[rownum] <- 'Covariance matrix non-invertible'
          rownum <- rownum + 1
          next
        }
        
        # if the covariance is positive definite then run the tests
        else{
          n_reject_H0_global <- 0
          n_reject_H0_max <- 0
          
          # this is the parallel foreach loop
          results <- foreach(i=1:n_rep, .packages = "MASS", .combine = rbind) %dopar%{
            
            data_obj <- GenerateData(d = level2,
                                     n = sample_size,
                                     rho = correlation,
                                     sigma = variance,
                                     CovStruct = level1,
                                     mu0 = mean0,
                                     mu0_loc = mean0_location,
                                     effect_size = effect_size_param,
                                     loc = effect_placement,
                                     effect_direction = effect_dir,
                                     mean_vec_manual = NULL)
            
            data <- data_obj$data # extract only the data itself from the object
            
            
            global_test_pvalue <- GlobalTest_AOV(data)$p.value
            max_test_pvalue <- MaxTest_small_samples(data,mu0=mean0,n_iter=n_iter_maxtest)
            
            c(global_reject = as.integer(global_test_pvalue <= alpha),
              max_reject = as.integer(max_test_pvalue <= alpha))
            
          }
          
          n_reject_H0_global <- sum(results[, 1])
          n_reject_H0_max <- sum(results[, 2])
          
          power_global <- n_reject_H0_global/n_rep
          power_max <- n_reject_H0_max/n_rep
          
          
          simulation_1_table$power_global[rownum] <- power_global
          simulation_1_table$power_max[rownum] <- power_max
          
          
          rownum <- rownum + 1
        }
  }
}

write.csv(simulation_1_table, "/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SIMULATIONS/Simulation_4_parallel_table.csv")


# stop cluster
stopCluster(cl)
