library(data.table)

# general plan
# 1.) statify by covariance structure
# 2.) make the sample size on the x axis and the power on the y-axis
# 3.) make a box for each dimensionality (4 boxes)
# 4.) make a line for each effect size (5 lines x2 tests = a total of 10 lines per plot)
# make sure that the ylims are the same for all plots



# visualization simulation 1 _______________________________________________________________________________
data <- fread('C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SIMULATIONS/FINAL_SIMULATIONS/FinalSim4.results.csv')
data$V1 <- NULL
AR1 <- data[data$var_covariance_structures == 'AR(1)',]
AR2 <- data[data$var_covariance_structures == 'AR(2)',]
Toeplitz <- data[data$var_covariance_structures == 'Toeplitz',]

View(data)

# AR(1)...............................................................................................

# stratify by dimensionalities
AR1_dim10 <- AR1[AR1$var_dimensionalities == 10,]
AR1_dim20 <- AR1[AR1$var_dimensionalities == 20,]
AR1_dim30 <- AR1[AR1$var_dimensionalities == 30,]
AR1_dim50 <- AR1[AR1$var_dimensionalities == 50,]


# stratify by effect sizes
AR1_dim10_eff001 <- AR1_dim10[AR1_dim10$var_effectsizes == 0.01, ]
AR1_dim10_eff003 <- AR1_dim10[AR1_dim10$var_effectsizes == 0.03, ]
AR1_dim10_eff005 <- AR1_dim10[AR1_dim10$var_effectsizes == 0.05, ]
AR1_dim10_eff007 <- AR1_dim10[AR1_dim10$var_effectsizes == 0.07, ]
AR1_dim10_eff01 <- AR1_dim10[AR1_dim10$var_effectsizes == 0.1, ]

AR1_dim20_eff001 <- AR1_dim20[AR1_dim20$var_effectsizes == 0.01, ]
AR1_dim20_eff003 <- AR1_dim20[AR1_dim20$var_effectsizes == 0.03, ]
AR1_dim20_eff005 <- AR1_dim20[AR1_dim20$var_effectsizes == 0.05, ]
AR1_dim20_eff007 <- AR1_dim20[AR1_dim20$var_effectsizes == 0.07, ]
AR1_dim20_eff01 <- AR1_dim20[AR1_dim20$var_effectsizes == 0.1, ]

AR1_dim30_eff001 <- AR1_dim30[AR1_dim30$var_effectsizes == 0.01, ]
AR1_dim30_eff003 <- AR1_dim30[AR1_dim30$var_effectsizes == 0.03, ]
AR1_dim30_eff005 <- AR1_dim30[AR1_dim30$var_effectsizes == 0.05, ]
AR1_dim30_eff007 <- AR1_dim30[AR1_dim30$var_effectsizes == 0.07, ]
AR1_dim30_eff01 <- AR1_dim30[AR1_dim30$var_effectsizes == 0.1, ]

AR1_dim50_eff001 <- AR1_dim50[AR1_dim50$var_effectsizes == 0.01, ]
AR1_dim50_eff003 <- AR1_dim50[AR1_dim50$var_effectsizes == 0.03, ]
AR1_dim50_eff005 <- AR1_dim50[AR1_dim50$var_effectsizes == 0.05, ]
AR1_dim50_eff007 <- AR1_dim50[AR1_dim50$var_effectsizes == 0.07, ]
AR1_dim50_eff01 <- AR1_dim50[AR1_dim50$var_effectsizes == 0.1, ]



plot(AR1_dim10_eff001$var_samplesizes, AR1_dim10_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "AR(1), dimensionality: 10")

lines(AR1_dim10_eff003$var_samplesizes, AR1_dim10_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR1_dim10_eff005$var_samplesizes, AR1_dim10_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR1_dim10_eff007$var_samplesizes, AR1_dim10_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR1_dim10_eff01$var_samplesizes,  AR1_dim10_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(AR1_dim10_eff001$var_samplesizes, AR1_dim10_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(AR1_dim10_eff003$var_samplesizes, AR1_dim10_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR1_dim10_eff005$var_samplesizes, AR1_dim10_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR1_dim10_eff007$var_samplesizes, AR1_dim10_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR1_dim10_eff01$var_samplesizes, AR1_dim10_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()





plot(AR1_dim20_eff001$var_samplesizes, AR1_dim20_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "AR(1), dimensionality: 20")

lines(AR1_dim20_eff003$var_samplesizes, AR1_dim20_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR1_dim20_eff005$var_samplesizes, AR1_dim20_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR1_dim20_eff007$var_samplesizes, AR1_dim20_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR1_dim20_eff01$var_samplesizes,  AR1_dim20_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(AR1_dim20_eff001$var_samplesizes, AR1_dim20_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(AR1_dim20_eff003$var_samplesizes, AR1_dim20_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR1_dim20_eff005$var_samplesizes, AR1_dim20_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR1_dim20_eff007$var_samplesizes, AR1_dim20_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR1_dim20_eff01$var_samplesizes, AR1_dim20_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()
      

plot(AR1_dim30_eff001$var_samplesizes, AR1_dim30_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "AR(1), dimensionality: 30")

lines(AR1_dim30_eff003$var_samplesizes, AR1_dim30_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR1_dim30_eff005$var_samplesizes, AR1_dim30_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR1_dim30_eff007$var_samplesizes, AR1_dim30_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR1_dim30_eff01$var_samplesizes,  AR1_dim30_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(AR1_dim30_eff001$var_samplesizes, AR1_dim30_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(AR1_dim30_eff003$var_samplesizes, AR1_dim30_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR1_dim30_eff005$var_samplesizes, AR1_dim30_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR1_dim30_eff007$var_samplesizes, AR1_dim30_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR1_dim30_eff01$var_samplesizes, AR1_dim30_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()




plot(AR1_dim50_eff001$var_samplesizes, AR1_dim50_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "AR(1), dimensionality: 50")

lines(AR1_dim50_eff003$var_samplesizes, AR1_dim50_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR1_dim50_eff005$var_samplesizes, AR1_dim50_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR1_dim50_eff007$var_samplesizes, AR1_dim50_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR1_dim50_eff01$var_samplesizes,  AR1_dim50_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(AR1_dim50_eff001$var_samplesizes, AR1_dim50_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(AR1_dim50_eff003$var_samplesizes, AR1_dim50_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR1_dim50_eff005$var_samplesizes, AR1_dim50_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR1_dim50_eff007$var_samplesizes, AR1_dim50_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR1_dim50_eff01$var_samplesizes, AR1_dim50_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()


   
# Create an empty plot window
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))  # arbitrary limits

# Add the legend only
legend("topleft",
       legend = c("Effect = 0.01", 
                  "Effect = 0.03", 
                  "Effect = 0.05", 
                  "Effect = 0.07", 
                  "Effect = 0.10"),
       col = c("cyan", "cyan1", "cyan2", "cyan3", "cyan4"),
       lty = 1, lwd = 4,
       title = "Test: Global",
       cex = 1.2,
       box.lwd = 1)

# Add second legend for MCTP in same figure (optional)
legend("bottomright",
       legend = c("Effect = 0.01", 
                  "Effect = 0.03", 
                  "Effect = 0.05", 
                  "Effect = 0.07", 
                  "Effect = 0.10"),
       col = c("darkorange", "darkorange1", "darkorange2", "darkorange3", "darkorange4"),
       lty = 1, lwd = 4,
       title = "Test: MCTP",
       cex = 1.2,
       box.lwd = 1)



# Coordinates for custom legend entries
legend_x <- par("usr")[1] + 2    # adjust as needed for your layout
legend_y <- seq(0.9, 0.5, length.out = 5)

effects <- c("0.01", "0.03", "0.05", "0.07", "0.10")
colors_glob <- c("cyan", "cyan1", "cyan2", "cyan3", "cyan4")
colors_mctp <- c("darkorange", "darkorange1", "darkorange2", "darkorange3", "darkorange4")

# Draw two-colored lines manually
for (i in 1:5) {
  segments(x0 = legend_x, y0 = legend_y[i],
           x1 = legend_x + 1, y1 = legend_y[i],
           col = colors_glob[i], lwd = 4)
  segments(x0 = legend_x + 1, y0 = legend_y[i],
           x1 = legend_x + 2, y1 = legend_y[i],
           col = colors_mctp[i], lwd = 4)
  text(x = legend_x + 2.2, y = legend_y[i],
       labels = paste("Effect =", effects[i]), adj = 0, cex = 0.8)
}




# AR(2)............................................................................................

# stratify by dimensionalities
AR2_dim10 <- AR2[AR2$var_dimensionalities == 10,]
AR2_dim20 <- AR2[AR2$var_dimensionalities == 20,]
AR2_dim30 <- AR2[AR2$var_dimensionalities == 30,]
AR2_dim50 <- AR2[AR2$var_dimensionalities == 50,]


# stratify by effect sizes
AR2_dim10_eff001 <- AR2_dim10[AR2_dim10$var_effectsizes == 0.01, ]
AR2_dim10_eff003 <- AR2_dim10[AR2_dim10$var_effectsizes == 0.03, ]
AR2_dim10_eff005 <- AR2_dim10[AR2_dim10$var_effectsizes == 0.05, ]
AR2_dim10_eff007 <- AR2_dim10[AR2_dim10$var_effectsizes == 0.07, ]
AR2_dim10_eff01 <- AR2_dim10[AR2_dim10$var_effectsizes == 0.1, ]

AR2_dim20_eff001 <- AR2_dim20[AR2_dim20$var_effectsizes == 0.01, ]
AR2_dim20_eff003 <- AR2_dim20[AR2_dim20$var_effectsizes == 0.03, ]
AR2_dim20_eff005 <- AR2_dim20[AR2_dim20$var_effectsizes == 0.05, ]
AR2_dim20_eff007 <- AR2_dim20[AR2_dim20$var_effectsizes == 0.07, ]
AR2_dim20_eff01 <- AR2_dim20[AR2_dim20$var_effectsizes == 0.1, ]

AR2_dim30_eff001 <- AR2_dim30[AR2_dim30$var_effectsizes == 0.01, ]
AR2_dim30_eff003 <- AR2_dim30[AR2_dim30$var_effectsizes == 0.03, ]
AR2_dim30_eff005 <- AR2_dim30[AR2_dim30$var_effectsizes == 0.05, ]
AR2_dim30_eff007 <- AR2_dim30[AR2_dim30$var_effectsizes == 0.07, ]
AR2_dim30_eff01 <- AR2_dim30[AR2_dim30$var_effectsizes == 0.1, ]

AR2_dim50_eff001 <- AR2_dim50[AR2_dim50$var_effectsizes == 0.01, ]
AR2_dim50_eff003 <- AR2_dim50[AR2_dim50$var_effectsizes == 0.03, ]
AR2_dim50_eff005 <- AR2_dim50[AR2_dim50$var_effectsizes == 0.05, ]
AR2_dim50_eff007 <- AR2_dim50[AR2_dim50$var_effectsizes == 0.07, ]
AR2_dim50_eff01 <- AR2_dim50[AR2_dim50$var_effectsizes == 0.1, ]







plot(AR2_dim10_eff001$var_samplesizes, AR2_dim10_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "AR(2), dimensionality: 10")

lines(AR2_dim10_eff003$var_samplesizes, AR2_dim10_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR2_dim10_eff005$var_samplesizes, AR2_dim10_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR2_dim10_eff007$var_samplesizes, AR2_dim10_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR2_dim10_eff01$var_samplesizes,  AR2_dim10_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(AR2_dim10_eff001$var_samplesizes, AR2_dim10_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(AR2_dim10_eff003$var_samplesizes, AR2_dim10_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR2_dim10_eff005$var_samplesizes, AR2_dim10_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR2_dim10_eff007$var_samplesizes, AR2_dim10_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR2_dim10_eff01$var_samplesizes, AR2_dim10_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()





plot(AR2_dim20_eff001$var_samplesizes, AR2_dim20_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "AR(2), dimensionality: 20")

lines(AR2_dim20_eff003$var_samplesizes, AR2_dim20_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR2_dim20_eff005$var_samplesizes, AR2_dim20_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR2_dim20_eff007$var_samplesizes, AR2_dim20_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR2_dim20_eff01$var_samplesizes,  AR2_dim20_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(AR2_dim20_eff001$var_samplesizes, AR2_dim20_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(AR2_dim20_eff003$var_samplesizes, AR2_dim20_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR2_dim20_eff005$var_samplesizes, AR2_dim20_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR2_dim20_eff007$var_samplesizes, AR2_dim20_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR2_dim20_eff01$var_samplesizes, AR2_dim20_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()


plot(AR2_dim30_eff001$var_samplesizes, AR2_dim30_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "AR(2), dimensionality: 30")

lines(AR2_dim30_eff003$var_samplesizes, AR2_dim30_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR2_dim30_eff005$var_samplesizes, AR2_dim30_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR2_dim30_eff007$var_samplesizes, AR2_dim30_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR2_dim30_eff01$var_samplesizes,  AR2_dim30_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(AR2_dim30_eff001$var_samplesizes, AR2_dim30_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(AR2_dim30_eff003$var_samplesizes, AR2_dim30_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR2_dim30_eff005$var_samplesizes, AR2_dim30_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR2_dim30_eff007$var_samplesizes, AR2_dim30_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR2_dim30_eff01$var_samplesizes, AR2_dim30_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()




plot(AR2_dim50_eff001$var_samplesizes, AR2_dim50_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "AR(2), dimensionality: 50")

lines(AR2_dim50_eff003$var_samplesizes, AR2_dim50_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR2_dim50_eff005$var_samplesizes, AR2_dim50_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR2_dim50_eff007$var_samplesizes, AR2_dim50_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR2_dim50_eff01$var_samplesizes,  AR2_dim50_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(AR2_dim50_eff001$var_samplesizes, AR2_dim50_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(AR2_dim50_eff003$var_samplesizes, AR2_dim50_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR2_dim50_eff005$var_samplesizes, AR2_dim50_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR2_dim50_eff007$var_samplesizes, AR2_dim50_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR2_dim50_eff01$var_samplesizes, AR2_dim50_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()




# Toeplitz ....................................................................................................

# stratify by dimensionalities
Toeplitz_dim10 <- Toeplitz[Toeplitz$var_dimensionalities == 10,]
Toeplitz_dim20 <- Toeplitz[Toeplitz$var_dimensionalities == 20,]
Toeplitz_dim30 <- Toeplitz[Toeplitz$var_dimensionalities == 30,]
Toeplitz_dim50 <- Toeplitz[Toeplitz$var_dimensionalities == 50,]


# stratify by effect sizes
Toeplitz_dim10_eff001 <- Toeplitz_dim10[Toeplitz_dim10$var_effectsizes == 0.01, ]
Toeplitz_dim10_eff003 <- Toeplitz_dim10[Toeplitz_dim10$var_effectsizes == 0.03, ]
Toeplitz_dim10_eff005 <- Toeplitz_dim10[Toeplitz_dim10$var_effectsizes == 0.05, ]
Toeplitz_dim10_eff007 <- Toeplitz_dim10[Toeplitz_dim10$var_effectsizes == 0.07, ]
Toeplitz_dim10_eff01 <- Toeplitz_dim10[Toeplitz_dim10$var_effectsizes == 0.1, ]

Toeplitz_dim20_eff001 <- Toeplitz_dim20[Toeplitz_dim20$var_effectsizes == 0.01, ]
Toeplitz_dim20_eff003 <- Toeplitz_dim20[Toeplitz_dim20$var_effectsizes == 0.03, ]
Toeplitz_dim20_eff005 <- Toeplitz_dim20[Toeplitz_dim20$var_effectsizes == 0.05, ]
Toeplitz_dim20_eff007 <- Toeplitz_dim20[Toeplitz_dim20$var_effectsizes == 0.07, ]
Toeplitz_dim20_eff01 <- Toeplitz_dim20[Toeplitz_dim20$var_effectsizes == 0.1, ]

Toeplitz_dim30_eff001 <- Toeplitz_dim30[Toeplitz_dim30$var_effectsizes == 0.01, ]
Toeplitz_dim30_eff003 <- Toeplitz_dim30[Toeplitz_dim30$var_effectsizes == 0.03, ]
Toeplitz_dim30_eff005 <- Toeplitz_dim30[Toeplitz_dim30$var_effectsizes == 0.05, ]
Toeplitz_dim30_eff007 <- Toeplitz_dim30[Toeplitz_dim30$var_effectsizes == 0.07, ]
Toeplitz_dim30_eff01 <- Toeplitz_dim30[Toeplitz_dim30$var_effectsizes == 0.1, ]

Toeplitz_dim50_eff001 <- Toeplitz_dim50[Toeplitz_dim50$var_effectsizes == 0.01, ]
Toeplitz_dim50_eff003 <- Toeplitz_dim50[Toeplitz_dim50$var_effectsizes == 0.03, ]
Toeplitz_dim50_eff005 <- Toeplitz_dim50[Toeplitz_dim50$var_effectsizes == 0.05, ]
Toeplitz_dim50_eff007 <- Toeplitz_dim50[Toeplitz_dim50$var_effectsizes == 0.07, ]
Toeplitz_dim50_eff01 <- Toeplitz_dim50[Toeplitz_dim50$var_effectsizes == 0.1, ]







plot(Toeplitz_dim10_eff001$var_samplesizes, Toeplitz_dim10_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "Toeplitz, dimensionality: 10")

lines(Toeplitz_dim10_eff003$var_samplesizes, Toeplitz_dim10_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(Toeplitz_dim10_eff005$var_samplesizes, Toeplitz_dim10_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(Toeplitz_dim10_eff007$var_samplesizes, Toeplitz_dim10_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(Toeplitz_dim10_eff01$var_samplesizes,  Toeplitz_dim10_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(Toeplitz_dim10_eff001$var_samplesizes, Toeplitz_dim10_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(Toeplitz_dim10_eff003$var_samplesizes, Toeplitz_dim10_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(Toeplitz_dim10_eff005$var_samplesizes, Toeplitz_dim10_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(Toeplitz_dim10_eff007$var_samplesizes, Toeplitz_dim10_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(Toeplitz_dim10_eff01$var_samplesizes, Toeplitz_dim10_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()





plot(Toeplitz_dim20_eff001$var_samplesizes, Toeplitz_dim20_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "Toeplitz, dimensionality: 20")

lines(Toeplitz_dim20_eff003$var_samplesizes, Toeplitz_dim20_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(Toeplitz_dim20_eff005$var_samplesizes, Toeplitz_dim20_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(Toeplitz_dim20_eff007$var_samplesizes, Toeplitz_dim20_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(Toeplitz_dim20_eff01$var_samplesizes,  Toeplitz_dim20_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(Toeplitz_dim20_eff001$var_samplesizes, Toeplitz_dim20_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(Toeplitz_dim20_eff003$var_samplesizes, Toeplitz_dim20_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(Toeplitz_dim20_eff005$var_samplesizes, Toeplitz_dim20_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(Toeplitz_dim20_eff007$var_samplesizes, Toeplitz_dim20_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(Toeplitz_dim20_eff01$var_samplesizes, Toeplitz_dim20_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()


plot(Toeplitz_dim30_eff001$var_samplesizes, Toeplitz_dim30_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "Toeplitz, dimensionality: 30")

lines(Toeplitz_dim30_eff003$var_samplesizes, Toeplitz_dim30_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(Toeplitz_dim30_eff005$var_samplesizes, Toeplitz_dim30_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(Toeplitz_dim30_eff007$var_samplesizes, Toeplitz_dim30_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(Toeplitz_dim30_eff01$var_samplesizes,  Toeplitz_dim30_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(Toeplitz_dim30_eff001$var_samplesizes, Toeplitz_dim30_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(Toeplitz_dim30_eff003$var_samplesizes, Toeplitz_dim30_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(Toeplitz_dim30_eff005$var_samplesizes, Toeplitz_dim30_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(Toeplitz_dim30_eff007$var_samplesizes, Toeplitz_dim30_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(Toeplitz_dim30_eff01$var_samplesizes, Toeplitz_dim30_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()




plot(Toeplitz_dim50_eff001$var_samplesizes, Toeplitz_dim50_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1),
     xlab = "Sample Size", ylab = "Power", main = "Toeplitz, dimensionality: 50")

lines(Toeplitz_dim50_eff003$var_samplesizes, Toeplitz_dim50_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(Toeplitz_dim50_eff005$var_samplesizes, Toeplitz_dim50_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(Toeplitz_dim50_eff007$var_samplesizes, Toeplitz_dim50_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(Toeplitz_dim50_eff01$var_samplesizes,  Toeplitz_dim50_eff01$powers_glob,  lwd = 4, col = 'cyan4')

lines(Toeplitz_dim50_eff001$var_samplesizes, Toeplitz_dim50_eff001$powers_mctp, lwd = 4, col = 'darkorange')
lines(Toeplitz_dim50_eff003$var_samplesizes, Toeplitz_dim50_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(Toeplitz_dim50_eff005$var_samplesizes, Toeplitz_dim50_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(Toeplitz_dim50_eff007$var_samplesizes, Toeplitz_dim50_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(Toeplitz_dim50_eff01$var_samplesizes, Toeplitz_dim50_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()
