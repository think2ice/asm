dba <- c(1.2, 1.8, 1.4, 2.3, 1.7, 1, 1.4, 1.8, 1.7, 2)
dbb <- c(1.5, 1.7, 2.5, 2.3, 2.1, 1.9, 1.8, 1.8, 2, 1.6)
dba.mean <- mean(dba)
dbb.mean <- mean(dbb)
dba.std <- sd(dba)
dbb.std <- sd(dbb)
boxplot(dba)
boxplot(dbb)
var.test.value <- dba.std^2/dbb.std^2
dfa <- length(dba)-1
dfb <- length(dbb)-1
qf(0.025, 9, 9)
qf(0.975, 9, 9)

# as the var.test.value is in the range between the 2 fishers, we could accept 
# the H0
# it is easier to use var.test 
var.test(dba, dbb)

# looking at p.value: if p.value is greater than alpha we could not reject H0
Sp <- (dfa*dba.std^2 + dfb*dbb.std^2)/(dfa + dfb)
Sp <- sqrt(Sp)
mean.test.val <- (dba.mean - dbb.mean)/(Sp*sqrt( 1/length(dba) + 1/length(dbb)))
mean.test.val <- abs(mean.test.val)
qt(0.975, dfa + dfb)

# as mean.test.val is lower than qt, we could not reject the H0
# easier if we compute it with t.test
t.test(dba, dbb)

# By default it takes variances as not equal, but in this case we do not reject 
# that they are not equal so we can compute:
t.test(dba, dbb, var.equal = TRUE)

# another time we could compare p.value with alpha and not reject 
# the H0 if p.value is larger than alpha
# reading a t.test: t is the value calculated so as to compare with the condition
# to accept or reject the H0

# let's see how we can apply the test to a paired data:
t.test(dba, dbb, paired = TRUE)
dba.dbb.dif <- dba - dbb
t.test(dba.dbb.dif)
