# Using R, and the R-function pgamma, compute the values of the posterior probabilities
# P(\[Theta] <= 1.0 | Underscript[y, ~]^*) , P(\[Theta] <= 5.5 | Underscript[y, ~]^*) , 
# and P(1.0 < \[Theta] <= 5.5 | Underscript[y, ~]^*)
p1=pgamma(1.0,shape=11.11, rate=39.61)
p2=pgamma(5.5,shape=11.11, rate=39.61)
p3=pgamma(5.5,shape=11.11, rate=39.61)-pgamma(1.0,shape=11.11, rate=39.61)
p1
p2
p3

# !check
integrate(function(x){dgamma(x,shape=11.11, rate=39.61)}, 1, 5.5)

# Use the R-function qgamma to compute the values of the 2.5th and the 97.5th percentiles of the posterior distribution. 
# Provide an equal-tail 95% posterior credible interval for \[Theta]. Interpret your interval. 
# Does your interval have final precision? Explain.
c1=qgamma(2.5/100.0,shape=11.11, rate=39.61)
c2=qgamma(97.5/100.0,shape=11.11, rate=39.61)
c1
c2

# Use the R-function rgamma, and the seed 1379, to draw a Monte-Carlo pseudo-random sample of 
# size 1000 from the posterior distribution of \[Theta], and use the data to approximate the mean, 
# variance, an equal-tail 95% probability interval and a histogram, of the posterior distribution of \[Theta].
set.seed(1379)
n=1000
theta.sim.sample=rgamma(n,shape=11.11, rate=39.61)
mean(theta.sim.sample)
var(theta.sim.sample)
PI=quantile(theta.sim.sample, probs = c(0.025, 0.975))
PI
hist(theta.sim.sample)

# Let Subscript[\[Tau], 1] = log(\[Theta]). 
# Use your answer to part (xi) to draw a Monte-Carlo pseudo-random sample of size 1000 from the distribution of Subscript[\[Tau], 1] , 
# and use the data to approximate the mean, variance, and an equal-tail 99% probability interval, of Subscript[\[Tau], 1].
tau1=log(theta.sim.sample)
hist(tau1)
mean(tau1)
var(tau1)
PI=quantile(tau1, probs = c(0.005, 0.995))
PI

# Let Subscript[\[Tau], 2] = 3 \[Theta] + 2. 
# Use your answer to part (xi) to draw a Monte-Carlo pseudo-random sample of size 1000 from the distribution of Subscript[\[Tau], 2] , 
# and use the data to approximate the mean, variance, and an equal-tail 99% probability interval, of Subscript[\[Tau], 2].
tau2=3.0*theta.sim.sample+2.0
hist(tau2)
mean(tau2)
var(tau2)
PI=quantile(tau2, probs = c(0.005, 0.995))
PI





