source('C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB.R')
source('C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB2.R')
library(foreach)
library(doParallel)

# make cluster for parallel computing
cores <- detectCores()
cl <- makeCluster(floor(cores[1]-2))
registerDoParallel(cl)


n <- 10
d <- 18
n_runs <- 10
n_iter_mctp <- 10
log_file_path <- 'C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/logfile.log'

results <- foreach(i = 1:n_runs, .packages = "MASS", .combine = rbind) %dopar%{
  
  status <- paste0(i/n_runs*100, '%')
  write(status, file = log_file_path, append = TRUE)
  
  X <- matrix(rnorm(n*d,0,1), nrow = n, ncol = d)
  
  pvalue1 <- MaxTest_small_samples(X,0,n_iter_mctp)
  pvalue2 <- MaxTest_small_samples2(X,0,n_iter_mctp)
  
  c(pvalue1, pvalue2)
}

results <- data.frame(results)
colnames(results) <- c('pval_MCTPWB1', 'pval_MCTPWB2')
View(results)



# stop cluster
stopCluster(cl)