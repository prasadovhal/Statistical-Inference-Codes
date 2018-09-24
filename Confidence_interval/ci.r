x = c(13.55, 13.60, 14.05, 15.10, 15.25, 15.40, 15.45, 15.75, 16.25, 16.40, 16.45, 16.65)

nsims = 1000 # number of simulations

m = rep(NA,nsims) # empty vector to store medians
for (i in 1:nsims)
{  
	m[i] = median(sample(x,replace=TRUE))
}

quantile(m,c(0.025,0.975))

