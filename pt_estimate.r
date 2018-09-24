n = 100	#sample length
m = 10000  	#repeat experiment
mean(rexp(n)) 
mu.hat = replicate(m,mean(rexp(n)))
mean(mu.hat)	#expectated value E(x) which will help to find parameter is bias or not
hist(mu.hat,'FD')	#FD - band with of histogram,one method
#shape of histogram is estimate of density function
#as we increase n histogram it gives standard normal distribution which is shape of estimator

