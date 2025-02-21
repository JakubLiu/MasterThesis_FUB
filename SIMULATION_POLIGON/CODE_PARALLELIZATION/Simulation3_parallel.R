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


# ====================================================== SIMULATION 1 ============================================================

# variables______________________________________________________________________

covariance_structures <- c('Unstructured', 'AR(1)', 'AR(2)', 'Toeplitz')  # level0

step5_10 <- seq(from = 5, to = 10, by = 1)  # for small sample sizes do small steps
step_11_100 <- seq(from = 11, to = 100, by = 10)   # for large sample sizes to large steps
sample_sizes <- c(step5_10, step_11_100) # level 1

effect_sizes <- 1:3 # level 2

n_iters_maxtest <- seq(from = 10, to = 20, by = 5)  # level 3



# constants___________________________________________________________________
effect_placement <- 2
number_of_columns_under_effect <- 1
dimensionality <- 18
correlation <- 0.5
variance <- 8
mean0 <- 0
mean0_location <- 1
alpha <- 0.05
# alternative_pattern not relevant for this simulation
n_rep <- 10  # number of times the data is simulated under the given conditions
n_simul <- length(covariance_structures) * length(sample_sizes) * length(effect_sizes) * length(n_iters_maxtest)
n_total_simul <- n_simul*n_rep # number of total simulations taking into account the repetitions
n_variables <- 4
simulation_1_table <- data.frame(matrix(0, nrow = n_simul, ncol = n_variables + 2))
colnames(simulation_1_table) <- c('covariance_structures','sample_sizes', 'effect_sizes', 'n_iters_maxtest', 'power_global', 'power_max')

rownum <- 1
for(level0 in covariance_structures){
  for(level1 in sample_sizes){
    for(level2 in effect_sizes){
      for(level3 in n_iters_maxtest){
        
        status <- paste0(round(rownum/n_simul*100,3), '%')
        print(status)
        simulation_1_table$covariance_structures[rownum] <- level0
        simulation_1_table$sample_sizes[rownum] <- level1
        simulation_1_table$effect_sizes[rownum] <- level2
        simulation_1_table$n_iters_maxtest[rownum] <- level3
        
        # first create the covarince matrix to check if it is positive definite
        cov_matrix <- GenCovMatrix(d=dimensionality,
                                   rho=correlation,
                                   sigma=variance,
                                   CovStruct=level0)
        
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
            
            data_obj <- GenerateData(d = dimensionality,
                                     n = level1,
                                     rho = correlation,
                                     sigma = variance,
                                     CovStruct = level0,
                                     mu0 = mean0,
                                     mu0_loc = mean0_location,
                                     effect_size = level2,
                                     loc = effect_placement,
                                     effect_direction = 'bidirectional',
                                     mean_vec_manual = NULL)
            
            data <- data_obj$data # extract only the data itself from the object
            
            
            global_test_pvalue <- GlobalTest_AOV(data)$p.value
            max_test_pvalue <- MaxTest_small_samples(data,mu0=mean0,n_iter=level3)
            
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
  }
}

View(simulation_1_table)
#write.csv(simulation_1_table, "C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/SIMULATION_POLIGON/simulation_1_table_small.csv")

# stop cluster
stopCluster(cl)