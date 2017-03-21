set.seed(97531)
n=1000

#generate random sample for theta_1
theta1=rbeta(n,shape1=4.0, shape2=4.0)
hist(theta1)
mean(theta1)
median(theta1)

#generate random sample for theta_2
theta2=rbeta(n,shape1=3.0, shape2=3.0)
hist(theta2)
mean(theta2)
median(theta2)

#tau1,tau4
tau1=theta1-theta2
tau4=theta1*(1-theta2)/theta2/(1-theta1)
mean(tau1)
var(tau1)
hist(tau1)
PI=quantile(tau1, probs = c(0.025, 0.975))
PI

P=ecdf(tau4)
plot(P)
1-P(1.0)

P1.0=mean((tau4>1.0))
P1.0

PI=quantile(tau4, probs = c(0.025, 0.975))
PI
