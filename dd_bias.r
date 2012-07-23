
# set up a population simulation with and without noise
# and see if stats can recover density dependence signal

death=0.2
b.mu=log(0.5)     # b is birth rate
b.dd=0.01
totT=1000
initN=10

climate=rnorm(totT,0,0.5)  # randomly varying climate covariate

recruit=function(N,b.mu,clim.t,b.dd){
 out=N*exp(b.mu+clim.t-b.dd*N)         # Ricker type recruitment function
}

# population size vector
N=recruits.save=rep(NA,totT)
N[1]=initN
for(tt in 2:totT){
  Ndeaths=death*N[tt-1]
  Nrecruits=recruit(N[tt-1],b.mu,climate[tt-1],b.dd)
  N[tt]=N[tt-1]-Ndeaths+Nrecruits
  # recruits.save[tt]=Nrecruits
}

# do stats -------------------------------------------------------------------

# first, cut off burn in phase
N=N[(totT/10):totT]
lagN=N[1:length(N)-1]
climate=climate[(totT/10):(totT-1)]
#recruits.save=recruits.save[(totT/10+1):totT]

# add noise
#obsN=N+rnorm(length(N),0,10)    # normal variation around true N
obsN=exp(log(N)+rnorm(length(N),0,0.2))    # lognormal variation around true N
lagObsN=obsN[1:length(obsN)-1]

# calculate per capita growth rate
r.true=log(N[2:length(N)]/lagN)
r.obs=log(obsN[2:length(N)]/lagObsN)

# model true data
print(summary(lm(r.true~climate+lagN)))
# model observed data
print(summary(lm(r.obs~climate+lagObsN)))

par(mfrow=c(1,2))
plot(lagN,r.true,main="True N")
plot(lagObsN,r.obs,main="Obs N")


# partial regression models (compare R^2 for true data and observed data scenarios)

# climate covariate only
# model true data
print(summary(lm(r.true~lagN)))
# model observed data
print(summary(lm(r.obs~lagObsN)))

# density dependence (lagN) only
# model true data
print(summary(lm(r.true~climate)))
# model observed data
print(summary(lm(r.obs~climate)))

residuals=resid(lm(r.true~climate+lagN))
acf(residuals)
pacf(residuals)