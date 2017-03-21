

# use hash-marks ##### as FIRST characters in a line of R-code to put
# COMMENTS and annotate the code

# Learning about a single scalar parameter theta - using no data
# See example 3.1.1 on page 37-38 of the CJBH text for the 
# beta prior on theta
#

# hyper-parameters of the prior

a = 3; b = 1

# METHOD 3 - Approx. Learning about theta by numerical integrations, etc

theta.values = c(0.1,.6, 0.8)
theta.values

# Evaluate the prior density for fixed values of theta

# look-up R-documentation on the beta density itself

?dbeta

dbeta(theta.values, shape1=a, shape2=b)

#warning do NOT put a=3, b=1 in the arguments of dbeta

# Evaluate approximately prior CDF (cumulative Distribution Function) values

v1 = pbeta(theta.values, shape1=a, shape2=b)
v1

# Prior prob[0.6 < theta < 0.8]
v1[3] -v1[2]

#Evaluate approximately prior quantiles

q = c(0.025, 0.5, 0.975)
qbeta(q, shape1=a, shape2=b)

# METHOD 4 - Approx. Learning about theta by 
# Monte-Carlo simulation of the prior on theta

# a pseudo-random sample of size N = 1000 by computer SIMULATION
# from the prior of theta

set.seed(1234)
N= 1000

# look-up R-documentation on sampling from a beta distribution

?rbeta
theta.sim.sample = rbeta(N, shape1=a, shape2=b)

#warning do NOT put a=3, b=1 in the arguments of rbeta

# do not display theta.sim.sample unless you want to see all 1000 values

# a GRAPHICAL approximation to the prior density of theta

?hist

hist(theta.sim.sample)
hist(theta.sim.sample, freq = FALSE)

# approximate prior mean of theta is

t1 = mean(theta.sim.sample)
t1

# approximate prior variance of theta is

t2 = var(theta.sim.sample)
t2

# approximate prior standard deviation of theta is

t3 = sqrt(t2)
t3

# approximate prior 2.5th , 50th, 97.5th percentiles of theta are

t4 = quantile(theta.sim.sample, probs = c(0.025, 0.5, 0.975))
t4

# what if you make a syntactic mistake in coding t4?
# for example t4 = quantile(theta.sim.sample, probs = c(0.025, 0.5, 0.975)

# the equal-tailed 95% probability interval for theta is found using s4

# Question - How to change code for t4 to obtain
# the equal-tailed 90% probability interval for theta  

# approximate prob that theta is > 0.6

t5 = sum(theta.sim.sample > 0.6)/N
t5

# approximate prob that 0.6 < theta  < 0.8

t6 = sum( (theta.sim.sample > 0.6) & (theta.sim.sample < 0.8) )/N
t6

# Approx. Learning about tau_1 by 
# Monte-Carlo simulation of the prior on 
# INTEREST parameter tau_1 = sqrt(theta)

# a pseudo-random sample of size N = 1000 from the prior of tau_1

tau_1.sim.sample = sqrt(theta.sim.sample)

# a GRAPHICAL approximation to the prior density of tau_1

hist(tau_1.sim.sample, freq = FALSE)

# approximate prior mean of tau_1 is

s1 = mean(tau_1.sim.sample)
s1

# approximate prior variance of tau_1 is

s2 = var(tau_1.sim.sample)
s2

# approximate prior standard deviation of tau_1 is

s3 = sqrt(s2)
s3

# approximate prior 2.5th , 50th, 97.5th percentiles of tau_1 are

s4 = quantile(tau_1.sim.sample, probs = c(0.025, 0.5, 0.975))
s4

# what if you make a syntactic mistake in coding s4?
# for example s4 = quantile(tau_1.sim.sample, probs = c(0.025, 0.5, 0.975)

# a 95% probability interval for tau_1 is found using s4

# approximate prob that tau_1 is > 0.5

s5 = sum(tau_1.sim.sample > 0.5)/N
s5


# end of R-code 



