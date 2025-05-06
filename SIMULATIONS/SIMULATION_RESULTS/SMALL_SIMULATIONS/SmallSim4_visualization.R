library(data.table)

sim4 <- fread("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SIMULATIONS/SIMULATION_RESULTS/SMALL_SIMULATIONS/Simulation4_results.csv")

sim4$V1 <- NULL
sim4 <- sim4[sim4$sample_sizes >= 5,]
sim4_AR1 <- sim4[sim4$covariance_structures == 'AR(1)']
sim4_AR2 <- sim4[sim4$covariance_structures == 'AR(2)']
sim4_Toeplitz <- sim4[sim4$covariance_structures == 'Toeplitz']
sim4_Unst <- sim4[sim4$covariance_structures == 'Unstructured']


# AR(1) covariance structure
plot(sim4_AR1[sim4_AR1$dimensions == 2]$sample_sizes, sim4_AR1[sim4_AR1$dimensions == 2]$power_max,
     col = 'magenta', type = 'l', lwd = 4, ylim = c(0.0, 1.0),
     xlab = 'sample size', ylab = 'power', main = 'AR(1) covariance structure')

lines(sim4_AR1[sim4_AR1$dimensions == 91]$sample_sizes, sim4_AR1[sim4_AR1$dimensions == 91]$power_max,
      col = 'red', type = 'l', lwd = 4)

lines(sim4_AR1[sim4_AR1$dimensions == 2]$sample_sizes, sim4_AR1[sim4_AR1$dimensions == 2]$power_global,
      col = 'green', type = 'l', lwd = 4)

lines(sim4_AR1[sim4_AR1$dimensions == 91]$sample_sizes, sim4_AR1[sim4_AR1$dimensions == 91]$power_global,
      col = 'blue', type = 'l', lwd = 4)

grid()

legend("bottomright",                   
       legend = c('MCTPWB 2 dimensions', 'MCTPWB 91 dimensions', 'global test 2 dimensions', 'global test 91 dimensions'),  
       col = c("magenta", "red", "green", "blue"),          
       lty = 1,
       lwd = 4,
       cex = 0.8)     





# AR(2) covariance structure
plot(sim4_AR2[sim4_AR2$dimensions == 2]$sample_sizes, sim4_AR2[sim4_AR2$dimensions == 2]$power_max,
     col = 'magenta', type = 'l', lwd = 4, ylim = c(0.0, 1.0),
     xlab = 'sample size', ylab = 'power', main = 'AR(2) covariance structure')

lines(sim4_AR2[sim4_AR2$dimensions == 91]$sample_sizes, sim4_AR2[sim4_AR2$dimensions == 91]$power_max,
      col = 'red', type = 'l', lwd = 4)

lines(sim4_AR2[sim4_AR2$dimensions == 2]$sample_sizes, sim4_AR2[sim4_AR2$dimensions == 2]$power_global,
      col = 'green', type = 'l', lwd = 4)

lines(sim4_AR2[sim4_AR2$dimensions == 91]$sample_sizes, sim4_AR2[sim4_AR2$dimensions == 91]$power_global,
      col = 'blue', type = 'l', lwd = 4)

grid()

legend("bottomright",                   
       legend = c('MCTPWB 2 dimensions', 'MCTPWB 91 dimensions', 'global test 2 dimensions', 'global test 91 dimensions'),  
       col = c("magenta", "red", "green", "blue"),          
       lty = 1,
       lwd = 4,
       cex = 0.8)     





# Toeplitz covariance structure
plot(sim4_Toeplitz[sim4_Toeplitz$dimensions == 2]$sample_sizes, sim4_Toeplitz[sim4_Toeplitz$dimensions == 2]$power_max,
     col = 'magenta', type = 'l', lwd = 4, ylim = c(0.0, 1.0),
     xlab = 'sample size', ylab = 'power', main = 'Toeplitz covariance structure')

lines(sim4_Toeplitz[sim4_Toeplitz$dimensions == 91]$sample_sizes, sim4_Toeplitz[sim4_Toeplitz$dimensions == 91]$power_max,
      col = 'red', type = 'l', lwd = 4)

lines(sim4_Toeplitz[sim4_Toeplitz$dimensions == 2]$sample_sizes, sim4_Toeplitz[sim4_Toeplitz$dimensions == 2]$power_global,
      col = 'green', type = 'l', lwd = 4)

lines(sim4_Toeplitz[sim4_Toeplitz$dimensions == 91]$sample_sizes, sim4_Toeplitz[sim4_Toeplitz$dimensions == 91]$power_global,
      col = 'blue', type = 'l', lwd = 4)

grid()

legend("right",                   
       legend = c('MCTPWB 2 dimensions', 'MCTPWB 91 dimensions', 'global test 2 dimensions', 'global test 91 dimensions'),  
       col = c("magenta", "red", "green", "blue"),          
       lty = 1,
       lwd = 4,
       cex = 0.8)     



# No covariance structure (Unstructured covariance matrix)
plot(sim4_Unst[sim4_Unst$dimensions == 2]$sample_sizes, sim4_Unst[sim4_Unst$dimensions == 2]$power_max,
     col = 'magenta', type = 'l', lwd = 4, ylim = c(0.0, 1.0),
     xlab = 'sample size', ylab = 'power', main = 'Unstructured covariance structure')

lines(sim4_Unst[sim4_Unst$dimensions == 91]$sample_sizes, sim4_Unst[sim4_Unst$dimensions == 91]$power_max,
      col = 'red', type = 'l', lwd = 4)

lines(sim4_Unst[sim4_Unst$dimensions == 2]$sample_sizes, sim4_Unst[sim4_Unst$dimensions == 2]$power_global,
      col = 'green', type = 'l', lwd = 4)

lines(sim4_Unst[sim4_Unst$dimensions == 91]$sample_sizes, sim4_Unst[sim4_Unst$dimensions == 91]$power_global,
      col = 'blue', type = 'l', lwd = 4)

grid()

legend("topright",                   
       legend = c('MCTPWB 2 dimensions', 'MCTPWB 91 dimensions', 'global test 2 dimensions', 'global test 91 dimensions'),  
       col = c("magenta", "red", "green", "blue"),          
       lty = 1,
       lwd = 4,
       cex = 0.8)     

