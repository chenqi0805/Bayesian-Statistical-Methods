# Use the R-function gamma to compute 
# the value of the normalizing constant.
c <- 9.8^2/gamma(2.3)
c

# Use the R-function dgamma to compute the value 
# of Subscript[p, 1](\[Theta]) for 3 values, 0.05, 0.2, and 4.5, of \[Theta]. 
# Hint: In R, pass the values of the hyper-parameters by shape = 2.3, rate = 9.8.
v1=dgamma(0.05,shape=2.3, rate=9.8)
v2=dgamma(0.2,shape=2.3, rate=9.8)
v3=dgamma(4.5,shape=2.3, rate=9.8)
v1
v2
v3

# Use the R-function pgamma to compute the values of the prior probabilities
# P (\[Theta] <= 1.0),  P (\[Theta] <= 5.5), and P(1.0 < \[Theta] <= 5.5).
pgamma(1.0,shape=2.3, rate=9.8)
pgamma(5.5,shape=2.3, rate=9.8)
pgamma(5.5,shape=2.3, rate=9.8)-pgamma(1.0,shape=2.3, rate=9.8)
# !check
integrate(function(x){dgamma(x,shape=2.3, rate=9.8)}, 1, 5.5)

# Use the R-function qgamma to compute the value of the 2.5^th, 50^th and 97.5^th 
# percentiles of the prior distribution. Provide a 95% probability interval for \[Theta].
qgamma(2.5/100.0,shape=2.3, rate=9.8)
qgamma(50.0/100.0,shape=2.3, rate=9.8)
qgamma(97.5/100.0,shape=2.3, rate=9.8)

# Use the R-function rgamma, and the seed 1379, to draw a Monte-Carlo 
# pseudo- random sample of size 1000 from the prior distribution, and use 
# those values to estimate the mean, and median of \[Theta].
set.seed(1379)
n=1000
y=rgamma(n,shape=2.3, rate=9.8)
hist(y)
mean(y)
median(y)

# Use your answer to part (o) to draw a Monte-Carlo pseudo-random sample 
# of size 1000 from the distribution of Subscript[\[Tau], 1], and use the 
# data to estimate the mean, median, and a 95% probability interval,of 
# Subscript[\[Tau], 1].
tau=log(y)
hist(tau)
mean(tau)
median(tau)
PI=quantile(tau, probs = c(0.025, 0.975))
PI










