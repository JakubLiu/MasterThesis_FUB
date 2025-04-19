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


# ====================================================== SIMULATION 2 ============================================================

# variables______________________________________________________________________
# linear ==> y = 2x + 5
# convex ==> y = 2x^2 + 2x + 5
# concave ==> y = (-2)x^2 + 2x + 5
alternative_patterns <- list(c(0,2,5), c(2,2,5), c(-2,2,5)) # level 1
alternative_patterns_names <- c('linear', 'convex', 'concave')
sample_sizes_step1 <- seq(from = 5, to = 10, by = 1)   # smaller steps for smaller sample sizes
sample_sizes_step2 <- seq(from = 11, to = 100, by = 10)  # larger steps for larger sample sizes
sample_sizes <- c(sample_sizes_step1, sample_sizes_step2)  # level 2

# constants___________________________________________________________________
dimensionality <- 20   # the dimensionality can't be to low, because otheriwse the alternative pattern will not show
variance <- 8
mean0_location <- 1
n_iter_maxtest <- 10
alpha <- 0.05
n_rep <- 5  # number of times the data is simulated under the given conditions
n_variables <- 2
n_simul <- length(alternative_patterns) * length(sample_sizes)
n_total_simul <- n_simul*n_rep # number of total simulations taking into account the repetitions
simulation_2_table <- data.frame(matrix(0, nrow = n_simul, ncol = n_variables + 2))
colnames(simulation_2_table) <- c('alternative_patterns', 'sample_sizes', 'power_global', 'power_max')

rownum <- 1
loc <- 1
for(level1 in alternative_patterns){
  for(level2 in sample_sizes){
    
      status <- paste0(round(rownum/n_simul*100,3), '%')
      print(status)
      simulation_2_table$alternative_patterns[rownum] <- alternative_patterns_names[loc]
      simulation_2_table$sample_sizes[rownum] <- level2
        
      # this is the parallel foreach loop
      results <- foreach(i=1:n_rep, .packages = "MASS", .combine = rbind) %dopar%{
          
        group_means <- GenAltPattern(d = dimensionality,
                                     x2_coef = unlist(level1)[1],
                                     x1_coef = unlist(level1)[2],
                                     x0_coef = unlist(level1)[3])
        
        data <- gen_data_given_grp_means(grp_means = group_means,
                                         n = level2,
                                         var = variance)
          
          
        global_test_pvalue <- GlobalTest_AOV(data)$p.value
        max_test_pvalue <- MaxTest_small_samples(data,mu0=mean(data[,mean0_location]),n_iter=n_iter_maxtest)
          
        c(global_reject = as.integer(global_test_pvalue <= alpha),
          max_reject = as.integer(max_test_pvalue <= alpha))
          
      }
        
        n_reject_H0_global <- sum(results[, 1])
        n_reject_H0_max <- sum(results[, 2])
        
        power_global <- n_reject_H0_global/n_rep
        power_max <- n_reject_H0_max/n_rep
        
        
        simulation_2_table$power_global[rownum] <- power_global
        simulation_2_table$power_max[rownum] <- power_max
        
        
        rownum <- rownum + 1
  }
  loc <- loc + 1
}


View(simulation_2_table)
#write.csv(simulation_2_table, "C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/SIMULATION_POLIGON/simulation_2_table_small.csv")

# stop cluster
stopCluster(cl)