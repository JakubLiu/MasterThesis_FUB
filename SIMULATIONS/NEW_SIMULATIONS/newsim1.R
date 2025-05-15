library(foreach)
library(doParallel)

# make cluster for parallel computing
num_cores <- 50
cl <- makeCluster(num_cores)
registerDoParallel(cl)


# source for data generation functions
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/DataGenerationFunctions.R")

# source for maximum test
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/MCTP_WB.R")

# source for global test
source("/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/GlobalTest.R")


GenDataGivenMeans <- function(means,n){
  data <- mapply(function(mu) rnorm(n, mean = mu, sd = 1), mu = means)
  return(data)
}


means_ <- seq(from = 0.0, to = 0.2, by = 0.01)
sample_sizes <- seq(from = 5, to = 100, by = 5)
n_simul <- 1000
n_iter_mctp <- 1000
alpha <- 0.05
powers_mctp <- vector(length = length(sample_sizes))
powers_glob <- vector(length = length(sample_sizes))
k <- 1

for(n in sample_sizes){

  status <- paste0(k/length(sample_sizes)*100, '%')
  write(status, file = 'newsim1.log', append = TRUE)

  pvalues <- foreach(i=1:n_simul, .packages = "MASS", .combine = rbind) %dopar%{
    
    X <- GenDataGivenMeans(means_, n)
    
    pvalue_mctp <- MaxTest_small_samples(X,0,n_iter_mctp)
    pvalue_glob <- GlobalTest_AOV(X)$p.value
    c(pvalue_mctp, pvalue_glob)
  }
  
  powers_mctp[k] <- sum(as.integer(pvalues[,1] <= alpha))/n_simul
  powers_glob[k] <- sum(as.integer(pvalues[,2] <= alpha))/n_simul
  k <- k + 1
}


# stop the cluster
stopCluster(cl)



write(powers_mctp, file = "powers_mctp.txt")
write(powers_glob, file = "powers_glob.txt")

print('all done.')
