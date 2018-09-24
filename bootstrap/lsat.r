lsat <- c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
gpa <- c(3.39,3.30,2.81,3.03,3.44,3.07,3.00,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)
df <- cbind(lsat,gpa)
N <- nrow(df)
#df[sample(1:N,replace = T),]
#cor(df[sample(1:N,replace = T),])[1,2]
#bootstarp replications
plot(lsat,gpa)
B <- 1000
sampl = 0

for(i in 1:B)
{
	sampl[i] = cor(df[sample(1:N,replace = T),])[1,2]
}

theta.hat = sum((lsat - mean(lsat))*(gpa - mean(gpa))) / sqrt(sum((lsat - mean(lsat))^2)*sum((gpa-mean(gpa))^2))
#theta.hat <- 0.776
se.hat <- sd(sampl)
alpha <- 0.05

#confidence intervals
#normal based C.I.
T.hat.normal <- theta.hat  + 2 * c(-se.hat,se.hat)
cat("normal based confidence interval is ",T.hat.normal,"\n")

#percentile C.I.
quantiles <- quantile(sampl,probs = c(1 - (alpha/2.0), alpha/2.0))
T.hat.percentile <- quantiles
#T.hat.percentile <- c(0.46,0.96)
cat("percentile confidence interval is ",T.hat.percentile,"\n")

#pivotal C.I.
T.hat.pivotal <- 2 * theta.hat - quantiles
#T.hat.pivotal <- 2 * theta.hat - T.hat.percentile
cat("pivotal confidence interval is ",T.hat.pivotal,"\n")

x11()
hist(sampl, xlab = "Bootstrap Sampal")
abline(v = theta.hat,col = "blue")
arrows(T.hat.normal[1],0,x1 = T.hat.normal[2],y1 = 0,code = 3,col = "red",length = 0.15)
arrows(T.hat.percentile[1],0,x1 = T.hat.percentile[2],y1 = 0,code = 3,col = "blue",length = 0.15)
arrows(T.hat.pivotal[1],0,x1 = T.hat.pivotal[2],y1 = 0,code = 3,col = "green",length = 0.15)

print("Note : ")
print("Normal confidence interval is in red")
print("Percentile confidence interval is in blue")
print("pivotal confidence interval is in green")
print("blue line indicates estimated correlation")

