# Bayesian Inferences for θ,
# a related parameter, and future data - A conjugate (Beta) prior analysis

# See pages 51-52 of the Text by CJBH for some of the R commands described below.
# Suppose observed data are summarized as
#  16 favored Obama Care, out of n=48 constituents polled
# NOTE: A sufficient statistic, T=number among n who favor Obama Care, exists in this 
# sampling model.

# A priori, theta is believed to be within the interval [0.2 ,0.6] with high probability.
# Fit a prior density from the Beta(a,b) family to match this info - see lecture notes

# old way ( <- )to make assignments to objects in R
a.prior <- 9.2   
b.prior <- 13.8

# Current way ( = ) to make assignments to objects in R
a.prior = 9.2   
b.prior = 13.8

# you will find both = and <- used interchangeably in R-codes, etc.
# plotting p1(theta) - prior density

theta = seq(0,1,0.01)  # Discretize theta using a grid of size 0.01

prr.theta = dbeta(theta,shape1=a.prior,shape2=b.prior)

#print(cbind(theta, prr.theta))

par(mfrow=c(2,2))  # making two rows and two columns for plots

plot(theta,prr.theta, type="l", main=paste("Beta(a=",a.prior,",b=",b.prior,") prior"),
     xlab="theta",ylab="Prob",cex.main=0.8)

# Likelihood, p2(tstar|theta),of the actual data from Binomial(n, theta)
n = 48    ## number of people who were polled
tstar = 16    ##  actual number who favored Obama Care 
lik.theta = dbinom(tstar,size=n,prob=theta)

# Unnormalized Likelihood -kernel
lik.theta = (theta^tstar)*((1-theta)^(n-tstar))

plot(theta,lik.theta, type="l", col="blue", main="Binomial Likelihood",xlab="theta",ylab="",cex.main=0.8)

# Product of Likelihood and Prior, but only 
# kernel or unnormalized posterior of theta
unnorm.post.theta = lik.theta*prr.theta
plot(theta,unnorm.post.theta,type="l",main="Likelihood*Prior", col="purple", cex.main=0.8)

# The Posterior is a Beta distribution due to the conjugate structure
# of the Beta prior and Binomial likelihood
# The parameters of the Beta posterior distribution
a.post = tstar + a.prior   
b.post = (n-tstar)+ b.prior

a.post
b.post

post.theta = dbeta(theta,shape1=a.post, shape2=b.post)

plot(theta, post.theta, type="l", main=paste("Posterior: Beta distn (a=",
                                             a.post,",b=",b.post,")"),col="red", cex.main=0.8)

# posterior by itself #

# A square region is needed for the next Plot
quartz(width=4,height=4,pointsize=8)

plot(theta, post.theta, type="l",main=paste("Posterior: Beta distn (a=",
                                            a.post,",b=",b.post,")"),col="red", cex.main=0.8)

# Exact Posterior mean 
post.mean = a.post/(a.post+b.post)

# Exact Posterior mode  
post.mode = (a.post-1)/(a.post+b.post-2)

print(c("Post mean=", post.mean, "Post mode=", post.mode))

#
# Approximate inferences (the posterior, point estimates, 95% credible Interval, posterior 
#prob) 
# for theta by Direct Monte-Carlo Simulation - i.i.d sampler
# pseudo random sample of size N from the posterior distribution of theta
#

N = 1000
# Fix the seed in sampling the posterior
set.seed(12345)

sample.post.theta = rbeta(N,shape1=a.post,shape2=b.post)

# Approx. posterior dist of theta
# A square region is needed for the next Plot

quartz(width=4,height=4,pointsize=8)
hist(sample.post.theta, xlab = "theta", ylab = "frequency", main =
       "Approximate posterior distribution of theta 
     using N=1000 draws from the posterior")

# Approximate posterior mean, variance of theta

MC.est.post.mean.theta = mean(sample.post.theta)

MC.est.post.var.theta = var(sample.post.theta)

print(c("MC est of Post mean=", MC.est.post.mean.theta, "MC est of Post variance=", 
        MC.est.post.var.theta))

# Approx. 95% credible Interval for theta

print(quantile(sample.post.theta,probs = c(0.025,0.975)))

# What is the approx. Posterior probability that theta < 0.50?

post.prob0.5 = mean((sample.post.theta < 0.5))

post.prob0.5

# Approximate inferences (posterior median, 95% credible Interval, posterior prob) for theta by numerical methods

# Posterior median
post.med = qbeta(0.5,shape1=a.post,shape2=b.post)

print(c("approx Post median=", post.med))

#95% credible Interval for theta

print(c(qbeta(0.025,shape1=a.post,shape2=b.post), 
        qbeta(0.975,shape1=a.post,shape2=b.post) ) )

# approx Posterior probability that theta < 0.50 

pbeta(0.5,shape1=a.post,shape2=b.post)


#  INFERENCE FOR TAU BEGINS
# Use the pseudo random sample of size N from the posterior distribution of theta
# to get a pseudo random sample of size N from the posterior distribution of tau,
# the odds of favoring Obama Care.

sample.post.tau = sample.post.theta/(1-sample.post.theta)

# A square region is needed for the next Plot

quartz(width=4,height=4,pointsize=8)
hist(sample.post.tau, xlab = "tau", ylab = "frequency", main =
       "Approximate posterior distribution of tau 
     using N=1000 draws from the posterior")

#
# Approximate Bayesian Inferences for tau using by Monte-Carlo simulation -
# pseudo random sample of size N from the posterior distribution of tau
#

# A point estimate of tau 
MC.approx.post.mean.tau = mean(sample.post.tau)

MC.approx.post.mean.tau

#95% credible Interval for tau
print(quantile(sample.post.tau,probs = c(0.025,0.975)))

# What is the (approximate) Posterior probability that tau >= 1.0?
post.prob1.0 = mean((sample.post.tau >= 1.0))

post.prob1.0

# Approximate predictive Bayesian inferences for W = # of people among the next 40 polled ># who favor Obama Care 
# by Monte-Carlo simulation - pseudo random 
# sample of size N2 from the posterior predictive distribution of W
m = 40    
N2 = length(sample.post.theta)
set.seed(12345)
sample.pred.W = rbinom(n=N2,size=m,prob= sample.post.theta)

# A square region is needed for the next Plot
quartz(width=4,height=4,pointsize=8)
hist(sample.pred.W, xlab = "W = # of people among 40 who favor Obama Care ", ylab = "frequency", main = 
       "Approximate posterior predictive distribution of W 
     using N=1000 draws from the posterior predictive")

# A point estimate of W 
MC.est.pred.mean.W = mean(sample.pred.W)

MC.est.pred.mean.W

# Approx. 95% posterior predictive Interval for W

print(quantile(sample.pred.W,probs = c(0.025,0.975)))

# What is the (approximate) posterior predictive probability that W >= 25?

pred.prob.25 = mean((sample.pred.W >= 25))

pred.prob.25

# Does the sampling model fit the data?
# check if tstar is or is not in the middle of the histogram of the predictive distribution of the 
# future datum,  W1 = # of people among the next 48 polled who would favor Obama Care

# Approximate predictive Bayesian inferences for W1 using a
# pseudo random sample of size N2 from the posterior predictive distribution of W1

m1 = 48    
N2 = length(sample.post.theta)
set.seed(12345)
sample.pred.W1 = rbinom(n=N2,size=m1,prob= sample.post.theta)

# A square region is needed for the next Plot
quartz(width=4,height=4,pointsize=8)
hist(sample.pred.W1, xlab = "W1 = # of people among 48 who favor Obama Care", ylab = "frequency", main =
       "Approximate posterior predictive distribution of W1
     using N=1000 draws from the posterior predictive") 
abline(v=tstar, lwd=3, col="red")
