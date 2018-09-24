mu = 2
sigma = 1

#X = rnorm(100,mu,sigma)
#mu_hat = mean(X)
#sigma_hat = sd(X)
mu_boot = replicate(1000,mean(rnorm(100,mu,sigma)))
sigma_boot = replicate(1000,sd(rnorm(100,mu,sigma)))

par(mfrow=c(2,2))
mean(mu_boot)
mean(sigma_boot)
hist(mu_boot)
hist(sigma_boot)
