# 1st deliverable of ASM

# Central limit theorem

# 1. Check the central limit theorem with Binomial samples

binomial.mean <- function(n, size, prob){
  bin <- rbinom(n, size, prob)
  mean(bin)
}
means <- replicate(1000, binomial.mean(15, 10,0.35))
hist(means)
means <- replicate(1000, binomial.mean(50, 10,0.35))
hist(means)

# 2. Check the central limit theorem with Poisson samples

poisson.mean <- function(n,lambda){
  poiss <- rpois(n, lambda)
  mean(poiss)
}
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
  1/(n-1)*sum((vec - m)^2)
}
var.cal2 <- function(vec){
  n <- length(vec)
  m <- mean(vec)
  1/n*sum((vec - m)^2)
}
s1 <- apply(norms, 1, var.cal1)
hist(s1)
s2 <- apply(norms, 1, var.cal2)
hist(s2)

# Concept of confidential interval

# 4. Confidence interval for mu in Normal distribution

n <- 30
norms <- replicate(100, rnorm(n, mean = 12, sd = sqrt(3)))
norms <- t(norms)
means <- apply(norms, 1, mean)
# TO DO: found S and add the correspondent t-student in lower.bound and upper.bound
S <- 
lower.bound <- mean(means) - S/sqrt(n)
upper.bound <- mean(means) + S/sqrt(n)
sum (means >= lower.bound && means <= upper.bound)
# 5. Confidence interval for sigma squared Normal distribution

n <- 30
norms <- replicate(100, rnorm(n, mean = 12, sd = sqrt(3)))
norms <- t(norms)
variances <- apply(norms, 1, var)
# TO DO: found S and add the correspondent Chi-squared in lower.bound and upper.bound
S <-
lower.bound <- mean(variances) - S/sqrt(n)
upper.bound <- mean(variances) + S/sqrt(n)
sum (variances >= lower.bound && variances <= upper.bound)

# Understand the difference between mean and median 

# 6. 
# Prove that mean minimizes the first expression (sum (xi-a)^2)
n <- 30 
# For example, we generate random data following a normal distribution 
data <- rnorm(n, mean = 12, sd = sqrt(3))
fun.min <- function(data,param){
  sum((data - param)^2)
}
mean.data <- as.integer(mean(data))
opt = optim(par = (mean.data-1):(mean.data+1), fn = fun.min, data = data)
opt$par
mean(data)

# Prove that median minimizes the second expression (sum(abs(xi-a)))
n <- 30 
# For example, we generate random data following a normal distribution 
data <- rnorm(n, mean = 12, sd = sqrt(3))
fun.min <- function(data,param){
  sum(abs(data - param))
}
med <- as.integer(median(data))
opt = optim(par = (med-1):(med+1), fn = fun.min, data = data)
opt$par

