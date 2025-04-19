source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MAX_TEST/MaxTest_function.R")
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/GLOBAL_TEST_ANOVA/GlobalTest_AOV_function.R")
source("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/SIMULATION_POLIGON/patterns_in_the_alternative.R")


basic_sim_function <- function(alpha, sample_size, alternative_pattern, n_simul, n_iter_max, n_groups){
  
  n_reject_H0_global <- 0
  n_reject_H0_max <- 0
  
  for(i in 1:n_simul){
    
    # simulate data NOT under H0
    baselines <- seq(from = -floor(n_groups/2), to = floor(n_groups/2), by = 1)
    group_means <- 1:d
    
    if(alternative_pattern == 'convex'){
      a_param <- 0.1
      b_param <- 0.2
      c_param <- 1.0
    }
    else if(alternative_pattern == 'concave'){
      a_param <- -0.1
      b_param <- 0.2
      c_param <- 1.0
    }
    if(alternative_pattern == 'linear'){
      # set the linear function parameters (y = ax + b)
      a_param <- 0.2
      b_param <- 5.0
      
      # get the group means
      for(i in 1:d){
        group_means[i] <- linear_f(baselines[i], a_param, b_param)
      }
      
      # generate data with given group means
      data <- GenDataGivenMeans(n=sample_size, group_means = group_means, s_dev=1)
      
      # set mu0 to the 1st column of the generated data
      mu0 <- colMeans(data)[1]
    }
    else if(alternative_pattern == 'convex' || alternative_pattern == 'concave'){
      
      # set the b and c parameters for the quadratic function (y = ax^2 + bx + c)
      b_param <- 0.2
      c_param <- 1.0
      
      if(alternative_pattern == 'convex'){
        a_param <- 1.0 # for a convex pattern set a > 0
        
        # get the group means
        for(i in 1:d){
          group_means[i] <- quadratic_f(baselines[i], a_param, b_param, c_param)
        }
        
        # generate data with given group means
        data <- GenDataGivenMeans(n=sample_size, group_means = group_means, s_dev=1)
        
        # set mu0 to the 1st column of the generated data
        mu0 <- colMeans(data)[1]
      }
      
      else if(alternative_pattern == 'concave'){
        a_param <- -1.0 # for a concave pattern ser a < 0
        
        # get the group means
        for(i in 1:d){
          group_means[i] <- quadratic_f(baselines[i], a_param, b_param, c_param)
        }
        
        # generate data with given group means
        data <- GenDataGivenMeans(n=sample_size, group_means = group_means, s_dev=1)
        
        # set mu0 to the 1st column of the generated data
        mu0 <- colMeans(data)[1]
      }
    }
    
    # perform the two tests
    pvalue_global <- GlobalTest_AOV(data)$p.value
    pvalue_max <- MaxTest_small_samples(data,mu0,n_iter = n_iter_max)
    
    if(pvalue_global <= alpha){n_reject_H0_global <- n_reject_H0_global + 1}
    if(pvalue_max <= alpha){n_reject_H0_max <- n_reject_H0_max + 1}
    
  }
  
  power_global <- n_reject_H0_global/n_simul
  power_max <- n_reject_H0_max/n_simul
  
  list("pwr_global" = power_global, "pwr_max" = power_max)
}



sample_sizes <- 5:10

powers_global <- 1:length(sample_sizes)
powers_max <- 1:length(sample_sizes)

for(i in 1:length(sample_sizes)){
  
  pwr <- basic_sim_function(alpha = 0.05,
                            sample_size = sample_sizes[i],
                            alternative_pattern = 'concave',
                            n_simul = 100,
                            n_iter_max = 100,
                            n_groups = 18)
  
  powers_global[i] <- pwr$pwr_global
  powers_max[i] <- pwr$pwr_max
}

powers_global
powers_max

#powers_global
#powers_max

plot(sample_sizes, powers_global, col = 'blue', pch = 16, ylab = 'power', main = 'global test')
grid()
plot(sample_sizes, powers_max, col = 'red', pch = 16, ylab = 'power', main = 'max test')
grid()

baselines <- seq(from = -9, to = 9, by = 1)
baselines

d <- length(baselines)

grp_means <- 1:d

concave_params <- c(-0.1, 0.2, 1.0)  # y = ax^2 + bx + c

for(i in 1:d){grp_means[i] <- quadratic_f(baselines[i], concave_params[1], concave_params[2] , concave_params[3])}

data_concave <- GenDataGivenMeans(n=10, group_means = grp_means, s_dev=1)

plot(colMeans(data_concave), col = 'red',
     xlab = 'group number',
     ylab = 'group mean',
     main = 'concave alternative pattern',
     pch = 16)
grid()

# *******************************************************************************************************
# Add questions about how to compare these two tests when in the max test you explicitly
# set the mu0 and in the global test you dont set it.
#********************************************************************************************************