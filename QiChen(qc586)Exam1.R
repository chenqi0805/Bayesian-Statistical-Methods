# Perform inferences approximately by Monte Carlo simulation of size N = 1000 
# of relevant distributions, with random seed set at 97531:
set.seed(97531)
n=1000

# A plot of the posterior distribution of Subscript[\[Tau], 1].
theta1.sim.sample=rbeta(n,shape1=355.0, shape2=660.0)
theta2.sim.sample=rbeta(n,shape1=442.0, shape2=420.0)
tau1.sim.sample=theta1.sim.sample-theta2.sim.sample
hist(tau1.sim.sample)

# The equal-tail 95% credible interval for Subscript[\[Tau], 1].
PI=quantile(tau1.sim.sample, probs = c(0.025, 0.975))
PI

# The equal-tail 95% credible interval for Subscript[\[Tau], 4].
tau4.sim.sample=theta1.sim.sample*(1.0-theta2.sim.sample)/(theta2.sim.sample*(1.0-theta1.sim.sample))
PI=quantile(tau4.sim.sample, probs = c(0.025, 0.975))
PI

# The equal-tail 95% posterior predictive interval for the number of 
# people who would agree with the Supreme Court ruling among the next 
# 500 Protestants to be polled for their opinions. Interpret your answer 
# in the context of the Survey described above.
m1 = 500    
N2 = length(theta1.sim.sample)
pred.sim.sample = rbinom(n=N2,size=m1,prob= theta1.sim.sample)
PI=quantile(pred.sim.sample, probs = c(0.025, 0.975))
PI



