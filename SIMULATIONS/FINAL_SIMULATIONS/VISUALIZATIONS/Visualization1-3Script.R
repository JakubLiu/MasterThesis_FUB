# This script retuns 12 figures in total. Just change the simulation number.
# It will only work seamlessly for simulation 1-3. For simulation 4 minor adjustments
# in the effect sizes are needed.


library(data.table)


# visualization simulation 1 _______________________________________________________________________________
data <- fread('C:/Users/Qba Liu/Documents/STUDIA/BIOINF_MASTER_BERLIN/MASTER_THESIS/MasterThesis_FUB/SIMULATIONS/FINAL_SIMULATIONS/FinalSim3_modified.results.csv')
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



# AR(2) ..............................................................................................
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




#=============================================AR1=========================================


# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(AR1_dim10_eff001$var_samplesizes, AR1_dim10_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(AR1_dim10_eff003$var_samplesizes, AR1_dim10_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR1_dim10_eff005$var_samplesizes, AR1_dim10_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR1_dim10_eff007$var_samplesizes, AR1_dim10_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR1_dim10_eff01$var_samplesizes,  AR1_dim10_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(AR1_dim10_eff001$var_samplesizes, AR1_dim10_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(AR1_dim10_eff003$var_samplesizes, AR1_dim10_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR1_dim10_eff005$var_samplesizes, AR1_dim10_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR1_dim10_eff007$var_samplesizes, AR1_dim10_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR1_dim10_eff01$var_samplesizes, AR1_dim10_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(AR1_dim10_eff001$var_samplesizes, AR1_dim10_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(AR1_dim10_eff003$var_samplesizes, AR1_dim10_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(AR1_dim10_eff005$var_samplesizes, AR1_dim10_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(AR1_dim10_eff007$var_samplesizes, AR1_dim10_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(AR1_dim10_eff01$var_samplesizes, AR1_dim10_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for AR(1) covariance structure, dimensionality = 10", outer = TRUE, cex = 1.5, line = 1)


# ==================================================================================================

# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(AR1_dim20_eff001$var_samplesizes, AR1_dim20_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(AR1_dim20_eff003$var_samplesizes, AR1_dim20_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR1_dim20_eff005$var_samplesizes, AR1_dim20_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR1_dim20_eff007$var_samplesizes, AR1_dim20_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR1_dim20_eff01$var_samplesizes,  AR1_dim20_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(AR1_dim20_eff001$var_samplesizes, AR1_dim20_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(AR1_dim20_eff003$var_samplesizes, AR1_dim20_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR1_dim20_eff005$var_samplesizes, AR1_dim20_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR1_dim20_eff007$var_samplesizes, AR1_dim20_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR1_dim20_eff01$var_samplesizes, AR1_dim20_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(AR1_dim20_eff001$var_samplesizes, AR1_dim20_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(AR1_dim20_eff003$var_samplesizes, AR1_dim20_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(AR1_dim20_eff005$var_samplesizes, AR1_dim20_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(AR1_dim20_eff007$var_samplesizes, AR1_dim20_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(AR1_dim20_eff01$var_samplesizes, AR1_dim20_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for AR(1) covariance structure, dimensionality = 20", outer = TRUE, cex = 1.5, line = 1)



# ===========================================================================================

# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(AR1_dim30_eff001$var_samplesizes, AR1_dim30_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(AR1_dim30_eff003$var_samplesizes, AR1_dim30_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR1_dim30_eff005$var_samplesizes, AR1_dim30_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR1_dim30_eff007$var_samplesizes, AR1_dim30_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR1_dim30_eff01$var_samplesizes,  AR1_dim30_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(AR1_dim30_eff001$var_samplesizes, AR1_dim30_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(AR1_dim30_eff003$var_samplesizes, AR1_dim30_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR1_dim30_eff005$var_samplesizes, AR1_dim30_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR1_dim30_eff007$var_samplesizes, AR1_dim30_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR1_dim30_eff01$var_samplesizes, AR1_dim30_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(AR1_dim30_eff001$var_samplesizes, AR1_dim30_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(AR1_dim30_eff003$var_samplesizes, AR1_dim30_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(AR1_dim30_eff005$var_samplesizes, AR1_dim30_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(AR1_dim30_eff007$var_samplesizes, AR1_dim30_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(AR1_dim30_eff01$var_samplesizes, AR1_dim30_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for AR(1) covariance structure, dimensionality = 30", outer = TRUE, cex = 1.5, line = 1)

#=========================================================================================

# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(AR1_dim50_eff001$var_samplesizes, AR1_dim50_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(AR1_dim50_eff003$var_samplesizes, AR1_dim50_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR1_dim50_eff005$var_samplesizes, AR1_dim50_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR1_dim50_eff007$var_samplesizes, AR1_dim50_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR1_dim50_eff01$var_samplesizes,  AR1_dim50_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(AR1_dim50_eff001$var_samplesizes, AR1_dim50_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(AR1_dim50_eff003$var_samplesizes, AR1_dim50_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR1_dim50_eff005$var_samplesizes, AR1_dim50_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR1_dim50_eff007$var_samplesizes, AR1_dim50_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR1_dim50_eff01$var_samplesizes, AR1_dim50_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(AR1_dim50_eff001$var_samplesizes, AR1_dim50_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(AR1_dim50_eff003$var_samplesizes, AR1_dim50_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(AR1_dim50_eff005$var_samplesizes, AR1_dim50_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(AR1_dim50_eff007$var_samplesizes, AR1_dim50_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(AR1_dim50_eff01$var_samplesizes, AR1_dim50_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for AR(1) covariance structure, dimensionality = 50", outer = TRUE, cex = 1.5, line = 1)

# ================================================= AR2 ============================================================


# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(AR2_dim10_eff001$var_samplesizes, AR2_dim10_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(AR2_dim10_eff003$var_samplesizes, AR2_dim10_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR2_dim10_eff005$var_samplesizes, AR2_dim10_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR2_dim10_eff007$var_samplesizes, AR2_dim10_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR2_dim10_eff01$var_samplesizes,  AR2_dim10_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(AR2_dim10_eff001$var_samplesizes, AR2_dim10_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(AR2_dim10_eff003$var_samplesizes, AR2_dim10_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR2_dim10_eff005$var_samplesizes, AR2_dim10_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR2_dim10_eff007$var_samplesizes, AR2_dim10_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR2_dim10_eff01$var_samplesizes, AR2_dim10_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(AR2_dim10_eff001$var_samplesizes, AR2_dim10_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(AR2_dim10_eff003$var_samplesizes, AR2_dim10_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(AR2_dim10_eff005$var_samplesizes, AR2_dim10_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(AR2_dim10_eff007$var_samplesizes, AR2_dim10_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(AR2_dim10_eff01$var_samplesizes, AR2_dim10_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for AR(2) covariance structure, dimensionality = 10", outer = TRUE, cex = 1.5, line = 1)


# ==================================================================================================

# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(AR2_dim20_eff001$var_samplesizes, AR2_dim20_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(AR2_dim20_eff003$var_samplesizes, AR2_dim20_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR2_dim20_eff005$var_samplesizes, AR2_dim20_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR2_dim20_eff007$var_samplesizes, AR2_dim20_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR2_dim20_eff01$var_samplesizes,  AR2_dim20_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(AR2_dim20_eff001$var_samplesizes, AR2_dim20_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(AR2_dim20_eff003$var_samplesizes, AR2_dim20_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR2_dim20_eff005$var_samplesizes, AR2_dim20_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR2_dim20_eff007$var_samplesizes, AR2_dim20_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR2_dim20_eff01$var_samplesizes, AR2_dim20_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(AR2_dim20_eff001$var_samplesizes, AR2_dim20_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(AR2_dim20_eff003$var_samplesizes, AR2_dim20_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(AR2_dim20_eff005$var_samplesizes, AR2_dim20_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(AR2_dim20_eff007$var_samplesizes, AR2_dim20_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(AR2_dim20_eff01$var_samplesizes, AR2_dim20_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for AR(2) covariance structure, dimensionality = 20", outer = TRUE, cex = 1.5, line = 1)



# ===========================================================================================

# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(AR2_dim30_eff001$var_samplesizes, AR2_dim30_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(AR2_dim30_eff003$var_samplesizes, AR2_dim30_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR2_dim30_eff005$var_samplesizes, AR2_dim30_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR2_dim30_eff007$var_samplesizes, AR2_dim30_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR2_dim30_eff01$var_samplesizes,  AR2_dim30_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(AR2_dim30_eff001$var_samplesizes, AR2_dim30_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(AR2_dim30_eff003$var_samplesizes, AR2_dim30_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR2_dim30_eff005$var_samplesizes, AR2_dim30_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR2_dim30_eff007$var_samplesizes, AR2_dim30_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR2_dim30_eff01$var_samplesizes, AR2_dim30_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(AR2_dim30_eff001$var_samplesizes, AR2_dim30_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(AR2_dim30_eff003$var_samplesizes, AR2_dim30_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(AR2_dim30_eff005$var_samplesizes, AR2_dim30_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(AR2_dim30_eff007$var_samplesizes, AR2_dim30_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(AR2_dim30_eff01$var_samplesizes, AR2_dim30_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for AR(2) covariance structure, dimensionality = 30", outer = TRUE, cex = 1.5, line = 1)

#=========================================================================================

# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(AR2_dim50_eff001$var_samplesizes, AR2_dim50_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(AR2_dim50_eff003$var_samplesizes, AR2_dim50_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(AR2_dim50_eff005$var_samplesizes, AR2_dim50_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(AR2_dim50_eff007$var_samplesizes, AR2_dim50_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(AR2_dim50_eff01$var_samplesizes,  AR2_dim50_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(AR2_dim50_eff001$var_samplesizes, AR2_dim50_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(AR2_dim50_eff003$var_samplesizes, AR2_dim50_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(AR2_dim50_eff005$var_samplesizes, AR2_dim50_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(AR2_dim50_eff007$var_samplesizes, AR2_dim50_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(AR2_dim50_eff01$var_samplesizes, AR2_dim50_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(AR2_dim50_eff001$var_samplesizes, AR2_dim50_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(AR2_dim50_eff003$var_samplesizes, AR2_dim50_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(AR2_dim50_eff005$var_samplesizes, AR2_dim50_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(AR2_dim50_eff007$var_samplesizes, AR2_dim50_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(AR2_dim50_eff01$var_samplesizes, AR2_dim50_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for AR(2) covariance structure, dimensionality = 50", outer = TRUE, cex = 1.5, line = 1)


# =============================== Toeplitz =========================================================

# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(Toeplitz_dim10_eff001$var_samplesizes, Toeplitz_dim10_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(Toeplitz_dim10_eff003$var_samplesizes, Toeplitz_dim10_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(Toeplitz_dim10_eff005$var_samplesizes, Toeplitz_dim10_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(Toeplitz_dim10_eff007$var_samplesizes, Toeplitz_dim10_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(Toeplitz_dim10_eff01$var_samplesizes,  Toeplitz_dim10_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(Toeplitz_dim10_eff001$var_samplesizes, Toeplitz_dim10_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(Toeplitz_dim10_eff003$var_samplesizes, Toeplitz_dim10_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(Toeplitz_dim10_eff005$var_samplesizes, Toeplitz_dim10_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(Toeplitz_dim10_eff007$var_samplesizes, Toeplitz_dim10_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(Toeplitz_dim10_eff01$var_samplesizes, Toeplitz_dim10_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(Toeplitz_dim10_eff001$var_samplesizes, Toeplitz_dim10_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(Toeplitz_dim10_eff003$var_samplesizes, Toeplitz_dim10_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(Toeplitz_dim10_eff005$var_samplesizes, Toeplitz_dim10_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(Toeplitz_dim10_eff007$var_samplesizes, Toeplitz_dim10_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(Toeplitz_dim10_eff01$var_samplesizes, Toeplitz_dim10_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for Toeplitz covariance structure, dimensionality = 10", outer = TRUE, cex = 1.5, line = 1)


# ==================================================================================================

# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(Toeplitz_dim20_eff001$var_samplesizes, Toeplitz_dim20_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(Toeplitz_dim20_eff003$var_samplesizes, Toeplitz_dim20_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(Toeplitz_dim20_eff005$var_samplesizes, Toeplitz_dim20_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(Toeplitz_dim20_eff007$var_samplesizes, Toeplitz_dim20_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(Toeplitz_dim20_eff01$var_samplesizes,  Toeplitz_dim20_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(Toeplitz_dim20_eff001$var_samplesizes, Toeplitz_dim20_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(Toeplitz_dim20_eff003$var_samplesizes, Toeplitz_dim20_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(Toeplitz_dim20_eff005$var_samplesizes, Toeplitz_dim20_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(Toeplitz_dim20_eff007$var_samplesizes, Toeplitz_dim20_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(Toeplitz_dim20_eff01$var_samplesizes, Toeplitz_dim20_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(Toeplitz_dim20_eff001$var_samplesizes, Toeplitz_dim20_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(Toeplitz_dim20_eff003$var_samplesizes, Toeplitz_dim20_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(Toeplitz_dim20_eff005$var_samplesizes, Toeplitz_dim20_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(Toeplitz_dim20_eff007$var_samplesizes, Toeplitz_dim20_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(Toeplitz_dim20_eff01$var_samplesizes, Toeplitz_dim20_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for Toeplitz covariance structure, dimensionality = 20", outer = TRUE, cex = 1.5, line = 1)



# ===========================================================================================

# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(Toeplitz_dim30_eff001$var_samplesizes, Toeplitz_dim30_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(Toeplitz_dim30_eff003$var_samplesizes, Toeplitz_dim30_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(Toeplitz_dim30_eff005$var_samplesizes, Toeplitz_dim30_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(Toeplitz_dim30_eff007$var_samplesizes, Toeplitz_dim30_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(Toeplitz_dim30_eff01$var_samplesizes,  Toeplitz_dim30_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(Toeplitz_dim30_eff001$var_samplesizes, Toeplitz_dim30_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(Toeplitz_dim30_eff003$var_samplesizes, Toeplitz_dim30_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(Toeplitz_dim30_eff005$var_samplesizes, Toeplitz_dim30_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(Toeplitz_dim30_eff007$var_samplesizes, Toeplitz_dim30_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(Toeplitz_dim30_eff01$var_samplesizes, Toeplitz_dim30_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(Toeplitz_dim30_eff001$var_samplesizes, Toeplitz_dim30_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(Toeplitz_dim30_eff003$var_samplesizes, Toeplitz_dim30_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(Toeplitz_dim30_eff005$var_samplesizes, Toeplitz_dim30_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(Toeplitz_dim30_eff007$var_samplesizes, Toeplitz_dim30_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(Toeplitz_dim30_eff01$var_samplesizes, Toeplitz_dim30_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for Toeplitz covariance structure, dimensionality = 30", outer = TRUE, cex = 1.5, line = 1)

#=========================================================================================

# Set layout for 3 plots in 1 row
par(mfrow = c(1, 3),    # 1 row, 3 columns
    mar = c(4, 4, 3, 1),  # Inner margins: bottom, left, top, right
    oma = c(1, 1, 3, 1))  # Outer margins: bottom, left, top, right

# --- Plot 1: Global Test ---
plot(Toeplitz_dim50_eff001$var_samplesizes, Toeplitz_dim50_eff001$powers_glob,
     type = 'l', lwd = 4, col = 'cyan',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "Global test")
lines(Toeplitz_dim50_eff003$var_samplesizes, Toeplitz_dim50_eff003$powers_glob, lwd = 4, col = 'cyan1')
lines(Toeplitz_dim50_eff005$var_samplesizes, Toeplitz_dim50_eff005$powers_glob, lwd = 4, col = 'cyan2')
lines(Toeplitz_dim50_eff007$var_samplesizes, Toeplitz_dim50_eff007$powers_glob, lwd = 4, col = 'cyan3')
lines(Toeplitz_dim50_eff01$var_samplesizes,  Toeplitz_dim50_eff01$powers_glob,  lwd = 4, col = 'cyan4')
grid()

# --- Plot 2: MCTP + WB ---
plot(Toeplitz_dim50_eff001$var_samplesizes, Toeplitz_dim50_eff001$powers_mctp,
     type = 'l', lwd = 4, col = 'darkorange',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB")
lines(Toeplitz_dim50_eff003$var_samplesizes, Toeplitz_dim50_eff003$powers_mctp, lwd = 4, col = 'darkorange1')
lines(Toeplitz_dim50_eff005$var_samplesizes, Toeplitz_dim50_eff005$powers_mctp, lwd = 4, col = 'darkorange2')
lines(Toeplitz_dim50_eff007$var_samplesizes, Toeplitz_dim50_eff007$powers_mctp, lwd = 4, col = 'darkorange3')
lines(Toeplitz_dim50_eff01$var_samplesizes, Toeplitz_dim50_eff01$powers_mctp, lwd = 4, col = 'darkorange4')
grid()

# --- Plot 3: MCTP + WB Modified ---
plot(Toeplitz_dim50_eff001$var_samplesizes, Toeplitz_dim50_eff001$powers_mctp_modified,
     type = 'l', lwd = 4, col = 'darkolivegreen1',
     ylim = c(0, 1), xlab = "Sample Size", ylab = "Power",
     main = "MCTP + WB Modified")
lines(Toeplitz_dim50_eff003$var_samplesizes, Toeplitz_dim50_eff003$powers_mctp_modified, lwd = 4, col = 'darkolivegreen2')
lines(Toeplitz_dim50_eff005$var_samplesizes, Toeplitz_dim50_eff005$powers_mctp_modified, lwd = 4, col = 'darkolivegreen3')
lines(Toeplitz_dim50_eff007$var_samplesizes, Toeplitz_dim50_eff007$powers_mctp_modified, lwd = 4, col = 'darkolivegreen4')
lines(Toeplitz_dim50_eff01$var_samplesizes, Toeplitz_dim50_eff01$powers_mctp_modified, lwd = 4, col = 'darkolivegreen')
grid()

# --- Add shared title ---
mtext("Power curves for Toeplitz covariance structure, dimensionality = 50", outer = TRUE, cex = 1.5, line = 1)



