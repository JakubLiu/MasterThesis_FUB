source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB.R")
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB2.R")

library(foreach)
library(doParallel)

# make cluster for parallel computing
cores <- detectCores()
cl <- makeCluster(floor(cores[1]-2))
registerDoParallel(cl)


n_simul <- 100
n_iter_mctp <- 100
d <- 18
sample_sizes <- seq(from = 3, to = 2500, by = 10)
powers1 <- 1:length(sample_sizes)
powers2 <- 1:length(sample_sizes)
alpha <- 0.05
effect_size <- 0.1


for(i in 1:length(sample_sizes)){
  
  status <- paste0(i/length(sample_sizes)*100, '%')
  print(status)
  n <- sample_sizes[i]
  pvalues_1 <- 1:n_simul
  pvalues_2 <- 1:n_simul
  
  results <- foreach(j = 1:n_simul, .packages = "MASS", .combine = rbind) %dopar%{
    
    # generate data
    X1 <- matrix(rnorm(n*(d-1), 0, 1), nrow = n, ncol = (d-1)) # data under H0
    X2 <- matrix(rnorm(n*1, 0+effect_size, 1), nrow = n, ncol = 1)    # column under effect
    X <- cbind(X1,X2)  # no random shuffling of the columns, just block structure
    
    # perform the tests
    pvalue1 <- MaxTest_small_samples(data=X, mu0=0, n_iter = n_iter_mctp)
    pvalue2 <- MaxTest_small_samples2(data=X, mu0=0, n_iter = n_iter_mctp)
    
    c(as.integer(pvalue1 <= alpha), as.integer(pvalue2 <= alpha))
  }
  
  powers1[i] <- sum(results[,1])/n_simul
  powers2[i] <- sum(results[,2])/n_simul
}

# stop cluster
stopCluster(cl)

output <- data.frame(cbind(sample_sizes,powers1,powers2))
colnames(output) <- c('sample_size', 'MCTPWB1_power', 'MCTPWB2_power')
View(output2)

write.csv(output, "C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/SOURCECODE/Power_results.csv")

