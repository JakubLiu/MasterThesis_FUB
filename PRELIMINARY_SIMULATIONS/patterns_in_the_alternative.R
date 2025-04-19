source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MAX_TEST/MaxTest_function.R")
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/GLOBAL_TEST_ANOVA/GlobalTest_AOV_function.R")

# function definitions_________________________________________________________________________________
quadratic_f <- function(x,a,b,c){
  y = x^2*a + b*x + c
  return(y)
}

linear_f <- function(x,a,b){
  y <- a*x + b
  return(y)
}

# function to generate data with the given group means
GenDataGivenMeans <- function(n,group_means,s_dev){
  
  d <- length(group_means)
  data <- matrix(rep(0,n*d), nrow=n, ncol=d)
  
  for(i in 1:d){
    data[,i] <- rnorm(n,group_means[i],s_dev)
  }
  
  return(data)
}

# # concave pattern in the alternative____________________________________________
# 
# # a nice way to check how the curve will look like
# curve(0.1*x^2 + 0.2*x + 1, from=-100, to=100, col = 'blue', lwd = 5)
# 
# # quadratic function parameters based on the look of curve()
# a_param <- 0.1
# b_param <- 0.2
# c_param <- 1
# 
# n <- 10
# baselines <- seq(from = -100, to = 100, by = 5) # arguments for quadratic f
# d <- length(baselines)
# group_means <- 1:d
# 
# # create the group means based on the baselines
# for(i in 1:d){
#   group_means[i] <- quadratic_f(baselines[i], a_param, b_param, c_param)
# }
# 
# 
# # simulation for concave alternartive pattern
# n_simul <- 1000
# n_reject <- 0
# 
# for(i in 1:n_simul){
#   
#   # generate concave data
#   data <- GenDataGivenMeans(n=10, group_means = group_means, s_dev=1)
#   
#   #set mu0 as the mean of the 1st group
#   mu0 <- colMeans(data)[1]
#   
#   # then deploy the tests and look at the power
# }
# 
# 
# # convex pattern in the alternative______________________________________________________________
# curve(-0.1*x^2 + 0.2*x + 1, from=-100, to=100, col = 'blue', lwd = 5)
# a_param <- -0.1
# b_param <- 0.2
# c_param <- 1
# 
# group_means_convex <- 1:d
# for(i in 1:d){
#   group_means_convex[i] <- quadratic_f(baselines[i], a_param, b_param, c_param)
# }
# 
# 
# # simulation for concave alternartive pattern
# n_simul <- 1000
# n_reject <- 0
# 
# for(i in 1:n_simul){
#   
#   # generate concave data
#   data <- GenDataGivenMeans(n=10, group_means = group_means_convex, s_dev=1)
#   
#   #set mu0 as the mean of the 1st group
#   mu0 <- colMeans(data)[1]
#   
#   # then deploy the tests and look at the power
# }
# 
# 
# # linear pattern in the alternative________________________________________________________________________
# curve(0.2*x + 5, from = -100, to = 100, col = 'red', lwd = 5)
# 
# a_param_lin <- 0.2
# b_param_lin <- 5.0
# 
# group_means_linear <- 1:d
# for(i in 1:d){
#   group_means_linear[i] <- linear_f(baselines[i], a_param_lin, b_param_lin)
# }
# 
# 
# # simulation for concave alternartive pattern
# n_simul <- 1000
# n_reject <- 0
# 
# for(i in 1:n_simul){
#   
#   # generate concave data
#   data <- GenDataGivenMeans(n=10, group_means = group_means_linear, s_dev=1)
#   
#   #set mu0 as the mean of the 1st group
#   mu0 <- colMeans(data)[1]
#   
#   # then deploy the tests and look at the power
# }
