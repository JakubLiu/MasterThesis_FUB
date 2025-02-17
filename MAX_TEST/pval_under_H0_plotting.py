import numpy as np
from matplotlib import pyplot as plt

dat = np.loadtxt('pvalues_under_H0.txt')
dat = dat.reshape(1000,1)

print(dat.shape)

plt.hist(dat, bins=50)
plt.xlabel('pvalue')
plt.ylabel('count')
plt.title('Histogram of pvalues under H0 from the Max Test with wild bootstrap')
plt.grid()
plt.show()