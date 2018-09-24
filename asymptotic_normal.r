n = 10000
x = replicate(n,mean(rexp(n)))
xbar = replicate(n,mean(rexp(n)))
hist(x,'FD',freq= F);curve(dnorm(x,mean(xbar),sd(xbar)),from=min(xbar),to=max(xbar),n=501,add=T,col="red")

