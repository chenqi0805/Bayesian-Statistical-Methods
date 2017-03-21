p1=pgamma(0.25,shape=11.11, rate=39.61)
p2=pgamma(0.35,shape=11.11, rate=39.61)
p3=pgamma(0.35,shape=11.11, rate=39.61)-pgamma(0.25,shape=11.11, rate=39.61)
p1
p2
p3

c1=qgamma(2.5/100.0,shape=11.11, rate=39.61)
c2=qgamma(97.5/100.0,shape=11.11, rate=39.61)
c1
c2

set.seed(1379)
n=1000
theta.sim.sample=rgamma(n,shape=11.11, rate=39.61)
tau1=log(theta.sim.sample)
hist(tau1)
mean(tau1)
var(tau1)
PI=quantile(tau1, probs = c(0.025, 0.975))
PI
