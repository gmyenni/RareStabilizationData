# set up a community simulation with and without noise
# and see if stats can recover frequency dependence signal

death=0.2
b.mu=log(0.5)     # b is birth rate
b.dd=0.01
totT=20
initN=10

death2=0.2
b.mu2=log(0.6)     # b is birth rate
b.dd2=0.02
initN2=10

climate=rnorm(totT,0,0.5)  # randomly varying climate covariate

recruit=function(N0,b.mu0,clim.t0,b.dd0){
 out=N0*exp(b.mu0+clim.t0-b.dd0*N0)         # Ricker type recruitment function
}

# population size vectors
N=initN
for(tt in 2:totT){
  Ndeaths=death*N[tt-1]
  Nrecruits=recruit(N[tt-1],b.mu,climate[tt-1],b.dd)
  N[tt]=N[tt-1]-Ndeaths+Nrecruits
  # recruits.save[tt]=Nrecruits
}

N2=initN2
for(tt in 2:totT){
  Ndeaths2=death2*N2[tt-1]
  Nrecruits2=recruit(N2[tt-1],b.mu2,climate[tt-1],b.dd2)
  N2[tt]=N2[tt-1]-Ndeaths2+Nrecruits2
  # recruits.save[tt]=Nrecruits
}

# do stats -------------------------------------------------------------------

# first, cut off burn in phase
N=N[(totT/10):totT]
lagN=N[1:length(N)-1]
N2=N2[(totT/10):totT]
lagN2=N2[1:length(N2)-1]
climate=climate[(totT/10):(totT-1)]
#recruits.save=recruits.save[(totT/10+1):totT]

# add noise
#obsN=N+rnorm(length(N),0,10)    # normal variation around true N
obsN=exp(log(N)+rnorm(length(N),0,0.2))    # lognormal variation around true N
lagObsN=obsN[1:length(obsN)-1]

#obsN2=N2+rnorm(length(N2),0,10)    # normal variation around true N
obsN2=exp(log(N2)+rnorm(length(N2),0,0.2))    # lognormal variation around true N
lagObsN2=obsN2[1:length(obsN2)-1]

# calculate per capita growth rate
r.true=log(N[2:length(N)]/lagN)
r.obs=log(obsN[2:length(N)]/lagObsN)

r.true2=log(N2[2:length(N2)]/lagN2)
r.obs2=log(obsN2[2:length(N2)]/lagObsN2)

#calculate relative abundance
TotN=N[1:length(N)-1]+N2[1:length(N)-1]
relN=N[1:length(N)-1]/TotN
relN2=N2[1:length(N)-1]/TotN
TotN.obs=obsN[1:length(obsN)-1]+obsN2[1:length(obsN)-1]
relN.obs=obsN[1:length(obsN)-1]/TotN.obs
relN2.obs=obsN2[1:length(obsN)-1]/TotN.obs

# model true data
print(summary(lm(r.true~climate+relN)))
# model observed data
print(summary(lm(r.obs~climate+relN.obs)))

par(mfrow=c(1,2))
plot(relN,r.true,main="True N")
plot(relN.obs,r.obs,main="Obs N")

# model true data
print(summary(lm(r.true2~climate+relN2)))
# model observed data
print(summary(lm(r.obs2~climate+relN2.obs)))

par(mfrow=c(1,2))
plot(relN2,r.true2,main="True N2")
plot(relN2.obs,r.obs2,main="Obs N2")

library("MCMCpack")
#Bayesian model to account for uncertainty
summary(MCMCregress(r.true~climate+relN, burnin = 1000, mcmc = 10000,
   thin = 1, verbose = 0, seed = NA, beta.start = NA, 
   b0 = 0, B0 = 1, c0 = 5, d0 = 5,
   marginal.likelihood = "none"))

summary(MCMCregress(r.obs~climate+relN.obs, burnin = 1000, mcmc = 10000,
   thin = 1, verbose = 0, seed = NA, beta.start = 0, 
   b0 = 0, B0 = 1, c0 = 5, d0 = 5,
   marginal.likelihood = "none"))

#Bayesian model to account for uncertainty
real2=as.data.frame(cbind(r.true2,climate,relN2,rand=rep(0,length(r.true2)),group=rep(1,length(r.true2))))

summary(MCMChregress(fixed=r.true2~climate+relN2, random= ~rand, data=real2, group = "group", 
    burnin = 1000, mcmc = 10000, thin = 10, verbose = 0, seed = NA, beta.start = 0, sigma2.start = NA,
    mubeta = 0, Vbeta=1.0E6, nu = 5, delta = 5, r = 2, R = diag(1,0.001))$mcmc)

data2=as.data.frame(cbind(r.obs2,climate,relN2.obs,rand=rep(0,length(r.true2)),group=rep(1,length(r.true2))))

summary(MCMChregress(fixed=r.obs2~climate+relN2.obs, random= ~rand, data=data2, group = "group", 
                     burnin = 1000, mcmc = 10000, thin = 10, verbose = 0, seed = NA, beta.start = 0, sigma2.start = NA,
                     mubeta = 0, Vbeta=1.0E6, nu = 5, delta = 5, r = 2, R = diag(1,0.001))$mcmc)
