source('/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/MCTP_WB.R')
source('/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/MCTP_WB2.R')

library(foreach)
library(doParallel)

# make cluster for parallel computing
cores <- detectCores()
cl <- makeCluster(cores[1]-8)  # leave 8 cores for other processes
registerDoParallel(cl)

n <- 10
d <- 18
n_runs <- 10000
n_iter_mctp <- 10000
nominal_alphas <- seq(from = 0.01, to = 0.1, by = 0.005)
type1_error_1 <- 1:length(nominal_alphas)
type1_error_2 <- 1:length(nominal_alphas)

for(i in 1:length(nominal_alphas)){

    status <- paste0(i/length(nominal_alphas)*100, '%')
    print(status)
    alpha <- nominal_alphas[i]

    results <- foreach(j = 1:n_runs, .packages = "MASS", .combine = rbind) %dopar%{
        
        X <- matrix(rnorm(n*d,0,1), nrow = n, ncol = d)  # generate data under H0
        pvalue1 <- MaxTest_small_samples(X,0,n_iter_mctp)
        pvalue2 <- MaxTest_small_samples2(X,0,n_iter_mctp)
        c(pvalue1, pvalue2)
    }

    type1_error_1[i] <- sum(as.integer(results[,1] <= alpha))/n_runs
    type1_error_2[i] <- sum(as.integer(results[,2] <= alpha))/n_runs
}

output <- data.frame(cbind(nominal_alphas, type1_error_1, type1_error_2))
write.csv(output, '/media/DANE/home/jliu/MASTER_THESIS_BER/SOURCECODE/Type1_error_sim_results.csv', row.names = FALSE)


# stop cluster
stopCluster(cl)