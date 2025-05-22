#Quadratic (concave) alternative pattern, variable effect size


#!/usr/bin/Rscript

source('/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/MCTP_WB_fast.R')  # MCTPWB
source('/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/GlobalTest.R')  # Global Test




# constants_____________________________________________________________________________
const_mu0 <- 0
const_alpha <- 0.05
const_nsim <- 10000
const_nboot <- 10000
const_samplesize <- 10
const_dimensionality <- 18


# variables___________________________________________________________________________
var_effectsize <- seq(from = 0.05, to = 0.5, by = 0.05)


f <- function(x,a,b,c){
  y <- a*x^2 + b*x + c
  return(y)
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
num_cores <- 60
cl <- makeCluster(num_cores)
registerDoParallel(cl)

powers_glob <- vector(length = length(var_effectsize))
powers_MCTP <- vector(length = length(var_effectsize))

for(i in 1:length(var_effectsize)){
  
  status <- paste0(i/length(var_effectsize)*100, '%')
  print(status)
  effectsize <- var_effectsize[i]
  means_linear <- seq(from = 1, to = 1 + effectsize, length.out = const_dimensionality)
  means_concave <- f(means_linear, -20, 41, -10)
  
  pvalues <- foreach(i=1:const_nsim, .packages = c("MASS", "resample"), .combine = rbind) %dopar%{
    
    X <- GenDataGivenMeans(means_concave,const_samplesize)
    pval_glob <- GlobalTest_AOV(X)$p.value
    pval_MCTP <- MCTPWB(X,mu0,const_nboot)
    c(pval_glob, pval_MCTP)
  }
  
  powers_glob[i] <- sum(as.integer(pvalues[,1] <= const_alpha))/const_nsim
  powers_MCTP[i] <- sum(as.integer(pvalues[,2] <= const_alpha))/const_nsim
}


sim_tab <- data.frame(cbind(var_effectsize,
                            powers_glob,
                            powers_MCTP))

# stop the cluster
stopCluster(cl)


write.csv(sim_tab, '/media/DANE/home/jliu/MASTER_THESIS_BER/SIMULATIONS/NEW_SIMULATIONS/Sim9.results.csv')

