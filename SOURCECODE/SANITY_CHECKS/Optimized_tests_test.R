source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB.R")  # MCTPWB
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB_optimized.R") # MCTPWB_opt
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB2.R")  # MCTPWB2
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB2_optimized.R")  # MCTPWB2_optim


library(foreach)
library(doParallel)

# make cluster for parallel computing
cores <- detectCores()
cl <- makeCluster(floor(cores[1]-2))
registerDoParallel(cl)

logfile_path <- "C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/Optimized_tests_test.log"
n <- 10
d <- 18
n_simul <- 1000
n_iter <- 1000

results <- foreach(i = 1:n_simul, .packages = "MASS", .combine = rbind) %dopar%{
  
  status <- paste0(i/n_simul*100, '%')
  write(status, file = logfile_path, append = TRUE)
  
  # generate data
  X <- matrix(rnorm(n*d,0,1), nrow = n, ncol = d)
  pval_MCTP <- MaxTest_small_samples(X,0,n_iter)
  pval_MCTP_opt <- MaxTest_small_samples_optim(X,0,n_iter)
  pval_MCTP2 <- MaxTest_small_samples2(X,0,n_iter)
  pval_MCTP2_opt <- MaxTest_small_samples_optim2(X,0,n_iter)
  c(pval_MCTP, pval_MCTP_opt, pval_MCTP2, pval_MCTP2_opt)
  
}

View(results)

# stop cluster
stopCluster(cl)


colnames(results) <- c("MCTP", "MCTP_opt", "MCTP2", "MCTP2_opt")
results <- data.frame(results)

# test if the pvalues from the tests and their optimized version are significantly different or not
t.test(results$MCTP, results$MCTP_opt)
t.test(results$MCTP2, results$MCTP2_opt)

write.csv(results, 'C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/Empirical_results_of_comp_optimized_vs_naive_tests.csv')
