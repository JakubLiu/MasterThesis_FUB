source('C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/MCTP_WB_fast.R')  # MCTPWB
source('C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SOURCECODE/GlobalTest.R')  # Global Test



# constants_____________________________________________________________________________
const_variance <- 1
const_dimensionality <- 18
const_samplesize <- 10
const_mu0 <- 0
const_alpha <- 0.05
const_nsim <- 10000
const_nboot <- 10000


# variables___________________________________________________________________________
var_effectsize <- seq(from = 0.01, to = 0.2, by = 0.01)



# function to generate data given the column means__________________________________
GenDataGivenMeans <- function(means,n){
  data <- mapply(function(mu) rnorm(n, mean = mu, sd = 1), mu = means)
  return(data)
}


# simulate________________________________________________________________________

library(foreach)
library(doParallel)

# make cluster for parallel computing
num_cores <- 12
cl <- makeCluster(num_cores)
registerDoParallel(cl)

powers_glob <- vector(length = length(var_effectsize))
powers_MCTP <- vector(length = length(var_effectsize))

for(i in 1:length(var_effectsize)){
  
  status <- paste0(i/length(var_effectsize)*100, '%')
  print(status)
  effect <- var_effectsize[i]
  
  pvalues <- foreach(i=1:const_nsim, .packages = c("MASS", "resample"), .combine = rbind) %dopar%{
    
    
    k <- 0
    means <- vector(length = const_dimensionality)
    for(j in 1:const_dimensionality){
      means[j] <- const_mu0 + effect*k
      k <- k + 1
    }
    
    X <- GenDataGivenMeans(means,const_samplesize)
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


write.csv(sim_tab, output_file_path)

plot(sim_tab$var_effectsize, sim_tab$powers_MCTP, col = 'red', type = 'l', lwd = 4, ylim = c(0,1),
     label = 'MCTPWB', xlab = 'sample size', ylab = 'power')
lines(sim_tab$var_effectsize, sim_tab$powers_glob, col = 'blue', type = 'l', lwd = 4, label = 'Global Test')
grid()

legend("bottomright",
       legend = c("MCTPWB", "Global Test"),
       col = c("red", "blue"),
       lwd = 4,
       bty = "n")
