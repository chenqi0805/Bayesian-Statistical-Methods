
## SDS384.7 Study Guide #4a, Fall 2016
## Pre (before drawing the MCMC samples) processing FEV data in R

# PART 1
## Perform EDA on FEV data set

fevstar.dat  = data.frame(read.table("/Users/chenyinglong/Documents/Bayesian Statistics Methods/FEV-Data-CJBH.txt", header=T))

attach(fevstar.dat)
fevstar.dat[1:3,]

ystar1 = subset(FEV, Smoke == 0)
length(ystar1)

ystar2 = subset(FEV, Smoke == 1)
length(ystar2)


#Side-by-side Box-plots
data.list = list(Nonsmoker = ystar1, Smoker = ystar2)
#quartz(width=4,height=4,pointsize=8)
boxplot(data.list, main="Side-by-side Box-plots of FEV Data", ylab="FEV", xlab = "")

# Regression - Scatter Plots
#quartz(width=8, height=8,pointsize=8)
plot(fevstar.dat)
title('Matrix of scatter plots - FEV Data')

## check on the propriety of the joint posterior
# An indirect check on the full-rank of the design matrix X in the regression model
fevstar.mat = cbind(rep(1, 345), Age, Smoke, Age*Smoke)
dim(fevstar.mat)

fevstar.mat[1:5,]

xprimex = crossprod(fevstar.mat)
dim(xprimex)

det(xprimex)

## end of EDA
## BDA begins

# PART 2

## Partial Remote processing to get MCMC samples

## Install first the R-package R2OpenBUGS

library(R2OpenBUGS)

setwd("/Users/chenyinglong/Documents/Bayesian Statistics Methods")
getwd()

## Model I

fev.dat1 =list(n=nrow(fevstar.dat), FEV=FEV, Smoke = Smoke)
bugs.data(data=fev.dat1, data.file="FEV-Regression-model1.txt")

# fev.dat =list(n=nrow(fevstar.dat),Age=Age, FEV=FEV, Smoke = Smoke)
# 
# bugs.data(data=fev.dat, data.file="FEV-Regression.txt")
# 
# ## Now leave R and launch OpenBUGS to load data manually using
# # FEV-Regression.txt; Open this text file from OpenBUGS and change file type as .txt
# 

# PART 3

# Full Remote processing to get MCMC samples
# without leaving R
 
OpenBUGS.pgm="/Users/chenyinglong/.wine/drive_c/Program\ Files/OpenBUGS/OpenBUGS323/OpenBUGS.exe"
 
set.seed(100)

library(R2OpenBUGS)

setwd("/Users/chenyinglong/Documents/Bayesian Statistics Methods")
getwd()

# fev.dat =list(n=nrow(fevstar.dat),Age=Age, FEV=FEV, Smoke = Smoke)
fev.dat1 =list(n=nrow(fevstar.dat), FEV=FEV, Smoke = Smoke)

#INITS - Chain 1
inits1 =list(tau=1.0, beta = c(0.0, 0.0))
#INITS - Chain 2
inits2 =list(tau= 5.0, beta = c(1.0, 2.0))
#INITS - Chain 3
inits3 =list(tau=10.0, beta = c(0.0, -2.0))


inits=list(inits1, inits2, inits3)


model.I.with.coda= bugs(data=fev.dat1,inits,OpenBUGS.pgm=OpenBUGS.pgm,model.file = "Model-I.txt",
                               n.burnin=500,parameters=c("post.prob.beta2","FEVs", "beta"),
                               n.chains = 3,n.iter=5500, codaPkg = TRUE, useWINE = TRUE)

## avoid this 
#print(model.fit.SG4a.with.coda,digits=4) #plot(model.fit.SG4a.with.coda)

## Post (after drawing the MCMC samples) processing in R to check for convergence of
## MCMC samples to the posteriors

library(coda)
library(lattice)
out.coda = read.bugs(model.I.with.coda)
class(out.coda)
str(out.coda)

#dev.off()
#quartz(width=4,height=4,pointsize=8)
xyplot(out.coda)
#quartz(width=4,height=4,pointsize=8)
densityplot(out.coda)


#Brooks-Gelman-Rubin-plot
#Work with samples from the posterior densities of nodes - ONE at a time
# beta4.sample.only = bugs(data=fev.dat1,inits,OpenBUGS.pgm=OpenBUGS.pgm,model.file = "OpenBUGS-txt-study-guide-4a-Fa16.txt",
#                          n.burnin=500,parameters=c("beta[2]"),
#                          n.chains = 3,n.iter=5500, codaPkg = TRUE, useWINE = TRUE)
beta.sample.only= bugs(data=fev.dat1,inits,OpenBUGS.pgm=OpenBUGS.pgm,model.file = "Model-I.txt",
                        n.burnin=500,parameters=c("beta"),
                        n.chains = 3,n.iter=5500, codaPkg = TRUE, useWINE = TRUE)
o1 = read.bugs(beta.sample.only)
#quartz(width=4,height=4,pointsize=8)
class(o1)
str(o1)
gelman.plot(o1)
# 
# ## Assuming that CODA analysis provides no serious concerns of non-convergence
# ## of MCMC samples to the posteriors we seek, call bugs function again in R2OpenBUGS
# ## with codaPkg = FALSE to produce node statistics, DIC, etc
# ## for ALL nodes defined in "parameters"
# 
model.I=bugs(data=fev.dat1,inits = inits,OpenBUGS.pgm=OpenBUGS.pgm,model.file = "Model-I.txt",
                    n.burnin=500,parameters=c("post.prob.beta2","FEVs","beta"),
                   n.chains = 3,n.iter=5500, bugs.seed = 12, codaPkg = FALSE, useWINE = TRUE)
# 
print(model.I,digits=4)
# quartz(width=4,height=4,pointsize=8)
# plot(model.I)