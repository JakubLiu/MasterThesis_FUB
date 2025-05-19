#!/usr/bin/Rscript

source('/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/MCTP_WB_fast.R')  # MCTPWB
source('/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/GlobalTest.R')  # Global Test


# constants_____________________________________________________________________________
const_mu0 <- 0
const_alpha <- 0.05
const_nsim <- 10000
const_nboot <- 10000


# variables___________________________________________________________________________
var_sample_sizes <- seq(from = 5, to = 100, by = 5)



# create the convec alternative pattern
means_linear <- seq(from = 1, to = 1.2, by = 0.01)

f <- function(x,a,b,c){
  y <- a*x^2 + b*x + c
  return(y)
}

means_convex <- f(means_linear, 15, -30, 1) # the parameters for the quadratic function where chosen by inspecting the scatterplots of the convex means



# function to generate data given the column means__________________________________
GenDataGivenMeans <- function(means,n){
  data <- mapply(function(mu) rnorm(n, mean = mu, sd = 1), mu = means)
  return(data)
}



# simulate________________________________________________________________________

library(foreach)
library(doParallel)

# make cluster for parallel computing
num_cores <- 70
cl <- makeCluster(num_cores)
registerDoParallel(cl)

powers_glob <- vector(length = length(var_sample_sizes))
powers_MCTP <- vector(length = length(var_sample_sizes))

for(i in 1:length(var_sample_sizes)){
  
  status <- paste0(i/length(var_sample_sizes)*100, '%')
  print(status)
  n <- var_sample_sizes[i]
  
  pvalues <- foreach(i=1:const_nsim, .packages = c("MASS", "resample"), .combine = rbind) %dopar%{
    
    X <- GenDataGivenMeans(means_convex,n)
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



write.csv(sim_tab, '/media/DANE/home/jliu/MASTER_THESIS_BER/SIMULATIONS/NEW_SIMULATIONS/Sim4.results.csv')