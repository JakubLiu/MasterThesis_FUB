source('C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB.R')
source('C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTPWB_new.R')
source('C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/GlobalTest.R')

library(foreach)
library(doParallel)

# make cluster for parallel computing
num_cores <- 10
cl <- makeCluster(num_cores)
registerDoParallel(cl)

sample_sizes <- seq(from = 5, to = 1000, by = 10)
n_simul <- 100
n_iter_max <- 100
alpha <- 0.05
n <- 10
d1 <- 10
d2 <- 10
eff_size_ <- 0.1
powers_v1 <- vector(length = length(sample_sizes))
powers_v2 <- vector(length = length(sample_sizes))
powers_glob <- vector(length = length(sample_sizes))
k <- 1

for(n in sample_sizes){
  print(paste0(k/length(sample_sizes)*100, '%'))
  pvalues <- vector(length = n_simul)
  
  pvalues <- foreach(i=1:n_simul, .packages = "MASS", .combine = rbind) %dopar%{
    
    X1 <- matrix(rnorm(n*d1,0,1), nrow = n, ncol = d1)
    X2 <- matrix(rnorm(n*d2,0+eff_size_,1), nrow = n, ncol = d2)
    X <- cbind(X1,X2)
    
    pvalue_v1 <- MaxTest_small_samples(X,0,n_iter_max)
    pvalue_v2 <- MaxTest_small_samples_new(X,0,n_iter_max)
    pvalue_glob <- GlobalTest_AOV(X)$p.value
    c(pvalue_v1, pvalue_v2, pvalue_glob)
  }
  
  powers_v1[k] <- sum(as.integer(pvalues[,1] <= alpha))/n_simul
  powers_v2[k] <- sum(as.integer(pvalues[,2] <= alpha))/n_simul
  powers_glob[k] <- sum(as.integer(pvalues[,3] <= alpha))/n_simul
  k <- k + 1
}


# stop the cluster
stopCluster(cl)


plot(sample_sizes, powers_v1, type = 'l', lwd = 4, col = 'red', ylim = c(0,1), label = 'MCTPWB1')
lines(sample_sizes, powers_v2, type = 'l', lwd = 4, col = 'green', label = 'MCTPWB2')
lines(sample_sizes, powers_glob, type = 'l', lwd = 4, col = 'blue', label = 'global test')
grid()


legend("bottomright",
       legend = c("MCTPWB1", "MCTPWB2", "Global Test"),
       col = c("red", "green", "blue"),
       lwd = 4,
       bty = "n")  # bty="n" removes the box around the legend