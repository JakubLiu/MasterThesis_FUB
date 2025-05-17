#!/usr/bin/Rscript

source('/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/MCTP_WB_fast.R')  # MCTPWB
source('data/cephfs-1/home/users/jali13_c/MASTER_THESIS/SOURCECODE/GlobalTest.R')  # Global Test



# constants_____________________________________________________________________________
const_variance <- 1
const_effectsize <- 0.03
const_dimensionality <- 18
const_mu0 <- 0
const_alpha <- 0.05
const_nsim <- 10000
const_nboot <- 10000


# variables___________________________________________________________________________
var_sample_sizes <- seq(from = 5, to = 100, by = 5)



# vector of (column) means__________________________________________________________
k <- 0
means <- vector(length = const_dimensionality)
for(i in 1:const_dimensionality){
  means[i] <- const_mu0 + const_effectsize*k
  k <- k + 1
}

# function to generate data given the column means__________________________________
GenDataGivenMeans <- function(means,n){
  data <- mapply(function(mu) rnorm(n, mean = mu, sd = 1), mu = means)
  return(data)
}


# simulate________________________________________________________________________

library(foreach)
library(doParallel)

# make cluster for parallel computing
num_cores <- 30
cl <- makeCluster(num_cores)
registerDoParallel(cl)

powers_glob <- vector(length = length(var_sample_sizes))
powers_MCTP <- vector(length = length(var_sample_sizes))

for(i in 1:length(var_sample_sizes)){

  status <- paste0(i/length(var_sample_sizes)*100, '%')
  print(status)
  n <- var_sample_sizes[i]

  pvalues <- foreach(i=1:const_nsim, .packages = c("MASS", "resample"), .combine = rbind) %dopar%{

        X <- GenDataGivenMeans(means,n)
        pval_glob <- GlobalTest_AOV(X)$p.value
        pval_MCTP <- MCTPWB(X,mu0,const_nboot)
        c(pval_glob, pval_MCTP)
  }

  powers_glob[i] <- sum(as.integer(pvalues[,1] <= const_alpha))/const_nsim
  powers_MCTP[i] <- sum(as.integer(pvalues[,2] <= const_alpha))/const_nsim
}


sim_tab <- data.frame(cbind(var_sample_sizes,
                            powers_glob,
                            powers_MCTP))

# stop the cluster
stopCluster(cl)


write.csv(sim_tab, '/data/cephfs-1/home/users/jali13_c/MASTER_THESIS/NEW_SIMULATIONS/Sim1.results.csv')
