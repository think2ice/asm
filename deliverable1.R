dnorm(3,5,1)
pnorm(3,5,1)
pnorm(0,0,1)

# Central limit theorem

# 1. Check the central limit theorem with Binomial samples

binomial.mean <- function(n, size, prob){
  bin <- rbinom(n, size, prob)
  mean(bin)
}
par(mfrow=c(2,1))
means <- replicate(1000, binomial.mean(15, 10,0.35))
hist(means)
means <- replicate(1000, binomial.mean(50, 10,0.35))
hist(means)

# 2. Check the central limit theorem with Poisson samples

poisson.mean <- function(n,lambda){
  poiss <- rpois(n, lambda)
  mean(poiss)
}
par(mfrow=c(2,1))
means <- replicate(1000, poisson.mean(15,6))
hist(means)
means <- replicate(1000, poisson.mean(50,6))
hist(means)

# answer: the means behave like a normal distribution


# Unbiased estimator
# 3. Check that to obtain an unbiased estimator of the theoretical variance, 
# we should divide by n-1 instead of by n

norms <- replicate(1000, rnorm(10, 30, sqrt(1.5)))
norms <- t(norms)
var.cal1 <- function(vec){
  n <- length(vec)
  m <- mean(vec)
  1/(n-1)*sum(vec - m)
}
var.cal2 <- function(vec){
  n <- length(vec)
  m <- mean(vec)
  1/n*sum(vec - m)
}
s1 <- apply(norms, 1, var.cal1)
hist(s1)
s2 <- apply(norms, 1, var.cal2)
hist(s2)
# Concept of confidential interval

# 4.
# 5. 

# Understand the difference between mean and median 

# 6. 