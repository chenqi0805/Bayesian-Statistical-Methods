# Set the random seed as 12345 in R and simulate 3 independent random samples, 
# one each from the above sampling models with the specifications below:
set.seed(12345)
n1=25
mu1=10
tau=1
ystar1=rnorm(n1,mean=mu1, sd=1/tau)
n2=35
mu2=30
tau=1
ystar2=rnorm(n2,mean=mu2, sd=1/tau)
n3=45
mu3=40
tau=1
ystar3=rnorm(n3,mean=mu3, sd=1/tau)

# EDA
# Side-by-side Box-plots
data.list = list(sim.sample1 = ystar1, sim.sample2 = ystar2, sim.sample3 = ystar3)
boxplot(data.list, main="Side-by-side Box-plots of 3 Samples of Data")

# Q-Q plot
# ystar1
qqnorm(ystar1, main= "Simulated Sample from \n from Normal population #1 \n Q-Q Plot") 
qqline(ystar1)
# ystar2
qqnorm(ystar2, main= "Simulated Sample from \n from Normal population #2 \n Q-Q Plot") 
qqline(ystar2)
# ystar3
qqnorm(ystar3, main= "Simulated Sample from \n from Normal population #3 \n Q-Q Plot") 
qqline(ystar3)

## end of EDA
## BDA begins

library(R2OpenBUGS)

setwd("/Users/chenyinglong/Documents/Bayesian Statistics Methods")
getwd()

## Problem 1

n = c(n1,n2,n3)
final.dat1 = list(n=n,y1=ystar1, y2=ystar2, y3=ystar3)
bugs.data(data=final.dat1, data.file = "3normalsamples.txt")

# Full Remote processing to get MCMC samples
# without leaving R

OpenBUGS.pgm="/Users/chenyinglong/.wine/drive_c/Program\ Files/OpenBUGS/OpenBUGS323/OpenBUGS.exe"

#inits - Chain 1 
inits1 =list(mu1=0,mu2=-1.0,mu3=0.1,tau= 1.0)
#inits - Chain 2 
inits2 =list(mu1=0,mu2=-1.0,mu3=0.2,tau= 0.2) 
#inits - Chain 3 
inits3 =list(mu1=-2.0,mu2=-1.0,mu3=0.5,tau= 0.3)

inits=list(inits1, inits2, inits3)

final.model1.with.coda= bugs(data=final.dat1,inits,OpenBUGS.pgm=OpenBUGS.pgm,model.file = "Final-model.txt",
                        n.burnin=500,parameters=c("delta","W", "post.prob.h0","pred.prob.y1.26","gamma"),
                        n.chains = 3,n.iter=5500, codaPkg = TRUE, useWINE = TRUE)

## Post (after drawing the MCMC samples) processing in R to check for convergence of
## MCMC samples to the posteriors

library(coda)
library(lattice)
out.coda = read.bugs(final.model1.with.coda)
class(out.coda)
str(out.coda)

#dev.off()
#quartz(width=4,height=4,pointsize=8)
xyplot(out.coda)
#quartz(width=4,height=4,pointsize=8)
densityplot(out.coda)

# Brooks-Gelman-Rubin-plot
# Work with samples from the posterior densities of nodes - ONE at a time
delta.sample.only= bugs(data=final.dat1,inits,OpenBUGS.pgm=OpenBUGS.pgm,model.file = "Final-model.txt",
                       n.burnin=500,parameters=c("delta"),
                       n.chains = 3,n.iter=5500, codaPkg = TRUE, useWINE = TRUE)
o1 = read.bugs(delta.sample.only)
#quartz(width=4,height=4,pointsize=8)
class(o1)
str(o1)
gelman.plot(o1)

# model without coda

final.model1=bugs(data=final.dat1,inits = inits,OpenBUGS.pgm=OpenBUGS.pgm,model.file = "Final-model.txt",
             n.burnin=500,parameters=c("delta","W", "post.prob.h0","pred.prob.y1.26","gamma"),
             n.chains = 3,n.iter=5500, bugs.seed = 12, codaPkg = FALSE, useWINE = TRUE)
# 
print(final.model1,digits=4)
