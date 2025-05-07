library(data.table)

sim7 <- fread("C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SIMULATIONS/SIMULATION_RESULTS/SMALL_SIMULATIONS/Simulation7_results.csv")
sim7$V1 <- NULL
View(sim7)
sim7 <- sim7[sim7$sample_sizes >= 5, ]


sim7_Unst <- sim7[sim7$covariance_structures == 'Unstructured', ]
sim7_AR1 <- sim7[sim7$covariance_structures == 'AR(1)', ]
sim7_AR2 <- sim7[sim7$covariance_structures == 'AR(2)', ]
sim7_Toe <- sim7[sim7$covariance_structures == 'Toeplitz', ]

# AR(1) covariance structure
plot(sim7_AR1[sim7_AR1$correlation_abs == 0.0, ]$sample_sizes, sim7_AR1[sim7_AR1$correlation_abs == 0.0, ]$power_max, type = 'l', lwd = 4,
     col = 'magenta', ylim = c(0.0,1.0), xlab = 'sample size', ylab = 'power', main = 'AR(1) covariance structure')
lines(sim7_AR1[sim7_AR1$correlation_abs == 0.9, ]$sample_sizes, sim7_AR1[sim7_AR1$correlation_abs == 0.9, ]$power_max, type = 'l', lwd = 4,
      col = 'red')
lines(sim7_AR1[sim7_AR1$correlation_abs == 0.1, ]$sample_sizes, sim7_AR1[sim7_AR1$correlation_abs == 0.1, ]$power_global, type = 'l', lwd = 4,
      col = 'green')
lines(sim7_AR1[sim7_AR1$correlation_abs == 0.9, ]$sample_sizes, sim7_AR1[sim7_AR1$correlation_abs == 0.9, ]$power_global, type = 'l', lwd = 4,
      col = 'blue')
legend("bottomright",                   
       legend = c('MCTPWB correlation 0.0', 'MCTPWB correlation 0.9', 'global test correlation 0.0', 'global test correlation 0.9'),  
       col = c("magenta", "red", "green", "blue"),          
       lty = 1,
       lwd = 4,
       cex = 0.8)   
grid()



# AR(2) covariance structure
plot(sim7_AR2[sim7_AR2$correlation_abs == 0.0, ]$sample_sizes, sim7_AR2[sim7_AR2$correlation_abs == 0.0, ]$power_max, type = 'l', lwd = 4,
     col = 'magenta', ylim = c(0.0,1.0), xlab = 'sample size', ylab = 'power', main = 'AR(2) covariance structure')
lines(sim7_AR2[sim7_AR2$correlation_abs == 0.9, ]$sample_sizes, sim7_AR2[sim7_AR2$correlation_abs == 0.9, ]$power_max, type = 'l', lwd = 4,
      col = 'red')
lines(sim7_AR2[sim7_AR2$correlation_abs == 0.1, ]$sample_sizes, sim7_AR2[sim7_AR2$correlation_abs == 0.1, ]$power_global, type = 'l', lwd = 4,
      col = 'green')
lines(sim7_AR2[sim7_AR2$correlation_abs == 0.9, ]$sample_sizes, sim7_AR2[sim7_AR2$correlation_abs == 0.9, ]$power_global, type = 'l', lwd = 4,
      col = 'blue')
legend("bottomright",                   
       legend = c('MCTPWB correlation 0.0', 'MCTPWB correlation 0.9', 'global test correlation 0.0', 'global test correlation 0.9'),  
       col = c("magenta", "red", "green", "blue"),          
       lty = 1,
       lwd = 4,
       cex = 0.8)   
grid()



# Toeplitz covariance structure
plot(sim7_Toe[sim7_Toe$correlation_abs == 0.0, ]$sample_sizes, sim7_Toe[sim7_Toe$correlation_abs == 0.0, ]$power_max, type = 'l', lwd = 4,
     col = 'magenta', ylim = c(0.0,1.0), xlab = 'sample size', ylab = 'power', main = 'Toeplitz covariance structure')
lines(sim7_Toe[sim7_Toe$correlation_abs == 0.9, ]$sample_sizes, sim7_Toe[sim7_Toe$correlation_abs == 0.9, ]$power_max, type = 'l', lwd = 4,
      col = 'red')
lines(sim7_Toe[sim7_Toe$correlation_abs == 0.1, ]$sample_sizes, sim7_Toe[sim7_Toe$correlation_abs == 0.1, ]$power_global, type = 'l', lwd = 4,
      col = 'green')
lines(sim7_Toe[sim7_Toe$correlation_abs == 0.9, ]$sample_sizes, sim7_Toe[sim7_Toe$correlation_abs == 0.9, ]$power_global, type = 'l', lwd = 4,
      col = 'blue')
legend("bottomright",                   
       legend = c('MCTPWB correlation 0.0', 'MCTPWB correlation 0.9', 'global test correlation 0.0', 'global test correlation 0.9'),  
       col = c("magenta", "red", "green", "blue"),          
       lty = 1,
       lwd = 4,
       cex = 0.8)   
grid()




# Unstructured covariance
plot(sim7_Unst[sim7_Unst$correlation_abs == 0.0, ]$sample_sizes, sim7_Unst[sim7_Unst$correlation_abs == 0.0, ]$power_max, type = 'l', lwd = 4,
     col = 'magenta', ylim = c(0.0,1.0), xlab = 'sample size', ylab = 'power', main = 'Unstructured covariance')
lines(sim7_Unst[sim7_Unst$correlation_abs == 0.9, ]$sample_sizes, sim7_Unst[sim7_Unst$correlation_abs == 0.9, ]$power_max, type = 'l', lwd = 4,
      col = 'red')
lines(sim7_Unst[sim7_Unst$correlation_abs == 0.1, ]$sample_sizes, sim7_Unst[sim7_Unst$correlation_abs == 0.1, ]$power_global, type = 'l', lwd = 4,
      col = 'green')
lines(sim7_Unst[sim7_Unst$correlation_abs == 0.9, ]$sample_sizes, sim7_Unst[sim7_Unst$correlation_abs == 0.9, ]$power_global, type = 'l', lwd = 4,
      col = 'blue')
legend("bottomright",                   
       legend = c('MCTPWB correlation 0.0', 'MCTPWB correlation 0.9', 'global test correlation 0.0', 'global test correlation 0.9'),  
       col = c("magenta", "red", "green", "blue"),          
       lty = 1,
       lwd = 4,
       cex = 0.8)   
grid()



