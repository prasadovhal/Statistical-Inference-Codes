library(e1071)

lsat <- c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
gpa <- c(3.39,3.30,2.81,3.03,3.44,3.07,3.00,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)
cor(lsat,gpa)
df <- cbind(lsat,gpa)
N <- nrow(df)
B <- 1000
alpha = 0.05
sampl = 0

Q1 <- function()
{
	sampl = replicate(B,cor(df1[sample(1:N,replace = T),])[1,2])

	theta.hat = cor(df1)[1,2]
	se.hat1 <- sd(sampl)
	T.hat.normal <- theta.hat  + 1.96 * c(-se.hat,se.hat)
	quantiles <- quantile(sampl,probs = c(1 - (alpha/2.0), alpha/2.0))
	T.hat.percentile <- quantiles
	T.hat.pivotal <- 2 * theta.hat - quantiles
	print(T.hat.normal)
	print(T.hat.percentile)
	print(T.hat.pivotal)
}

Q2 <- function()
{
	Y = gpa
	X = exp(Y)
	sampl = replicate(B,skewness(X[sample(1:N,replace = T)]))

	theta.hat = skewness(X)
	se.hat <- sd(sampl)
	T.hat.normal <- theta.hat  + 1.96 * c(-se.hat,se.hat)
	quantiles <- quantile(sampl,probs = c(1 - (alpha/2.0), alpha/2.0))
	T.hat.percentile <- quantiles
	T.hat.pivotal <- 2 * theta.hat - quantiles
	print(T.hat.normal)
	print(T.hat.percentile)
	print(T.hat.pivotal)
}

Q3 <- function()
{
	X = gpa
	dis = dt(X,3)
	q = quantile(dis,prob = c(0.75,0.25))
	theta.hat = (q[1] - q[2]) / 1.34
	for(i in 1:B)
	{
		X_sampl = X[sample(1:N,replace = T)]
		dis1 = dt(X_sampl,3)
		q1 = quantile(dis1,prob = c(0.75,0.25))
		sampl[i] = (q1[1] - q1[2])*100 / 1.34
	}

	
	se.hat <- sd(sampl)
	T.hat.normal <- theta.hat  + 1.96 * c(-se.hat,se.hat)
	quantiles <- quantile(sampl,probs = c(1 - (alpha/2.0), alpha/2.0))
	T.hat.percentile <- quantiles
	T.hat.pivotal <- 2 * theta.hat - quantiles
	print(T.hat.normal)
	print(T.hat.percentile)
	print(T.hat.pivotal)
}

Q4 <- function()
{
	X = rnorm(100,5,1)
	theta.hat = exp(mean(X))
	n = 100
	sampl = replicate(B,exp(mean(X[sample(1:n,replace = T)])))
	
	se.hat <- sd(sampl)
	T.hat.normal <- theta.hat  + 1.96 * c(-se.hat,se.hat)
	quantiles <- quantile(sampl,probs = c(1 - (alpha/2.0), alpha/2.0))
	T.hat.percentile <- quantiles
	T.hat.pivotal <- 2 * theta.hat - quantiles
	print(T.hat.normal)
	print(T.hat.percentile)
	print(T.hat.pivotal)
}

Q5 <- function()
{
	X = runif(100,0,10)
	theta.hat = max(X)
	n = 100
	sampl = replicate(B,max(X[sample(1:n,replace = T)]))
	
	se.hat <- sd(sampl)
	T.hat.normal <- theta.hat  + 1.96 * c(-se.hat,se.hat)
	quantiles <- quantile(sampl,probs = c(1 - (alpha/2.0), alpha/2.0))
	T.hat.percentile <- quantiles
	T.hat.pivotal <- 2 * theta.hat - quantiles
	print(T.hat.normal)
	print(T.hat.percentile)
	print(T.hat.pivotal)
}
