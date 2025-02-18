# Abstract
Scenarios where the sample size is small, and the dimensionality of the data is high are common especially in preclinical research. The high number of dimensions might arise due to the presence of repeated measures or many features. One cause of the abundance of small sample cases is the high cost of conducting specific laboratory experiments or the rarity of a given disease. This case is often referred to as the “high p, low n” case and provides a problem for the statistical analysis of the data for several reasons. One reason is that many methods tend not to control the type-I error-rate well when n is small. Moreover, many methods are based on strict distributional assumptions, which simply can’t be verified when the sample size isn’t large enough. Currently there are two main ways of analyzing high dimensional data. One of them is to use a global test (Rauf Ahmad, 2008 1). Which answers the question if there is a significant difference between any of the compared group means. The main disadvantage of this approach is that it does not offer insight into the actual location of the difference (i.e. exactly which two groups have statistically significantly different means). The alternative is the so-called Multiple Contrast Testing Procedure (MCTP) (Konietschke, 2021 2), which is based on a vector of test statistics. Each test statistics stems from a comparison of group means in a single dimension (or the comparison of the group mean in each dimension, to a reference value, in the case of a one sample test). The global null hypothesis is then rejected or not, based on the values of the “local” test statistics. Several implementations of MCTPs have been proposed, an example is the proposition of (Chang, 20173). The biggest problem that such procedures have to face is the difficulty in estimating the distribution of the test statistic when the number of observations is limited. (Konietschke, 2021 2) proposes an implementation of the Multiple Contrast Testing Procedure, where the distribution of the test statistic is estimated using a Wild Bootstrap approach (Konietschke, 2021 2). Simulations have shown that this method controls the type-I error-rate much better than the approach proposed by (Chang, 2017 3) when the sample size is very small. The aim of this thesis is to compare the statistical power of the global test (with the ANOVA type statistic) (Rauf Ahmad, 2008 1) and the MCTP (with a Wild Bootstrap approach) (Konietschke, 2021 2) under various conditions. The expectation is that the first method will exhibit superior power, since fewer parameters are being estimated (for example the lack of SCI (simultaneous confidence intervals) estimation). So, the research question can be formulated as “How high is the price to pay, for the additional information gained from using MCTPs?”. This question will be answered using extensive simulations and real data applications.

1Ahmad MR, Werner C, Brunner E. Analysis of high-dimensional repeated measures designs: The one sample case. Computational statistics & data analysis. 2008 Dec 15;53(2):416-27.

2Konietschke F, Schwab K, Pauly M. Small sample sizes: A big data problem in high-dimensional data analysis. Statistical Methods in Medical Research. 2021 Mar;30(3):687-701.

3Chang J, Zheng C, Zhou WX, Zhou W. Simulation‐based hypothesis testing of high dimensional means under covariance heterogeneity. Biometrics. 2017 Dec;73(4):1300-10.
