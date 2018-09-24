#x = 1:10 #simple calculations
x <- rnorm(10)
ss <-sample(x,length(x),replace=T)	#bootstrap replications of given data(x) from imperical distribution function

#estimator can be anything not just mean
print(mean(x))
print(mean(ss))


#package boot is used for bootstraping
#help(package = boot)
library(boot)
#statistic -> in boot is estimator
