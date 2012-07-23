  #specify combinations of freq-dep parameters
totT=100

  initN=5
  initN2=5

  r1=20
  r2=20

  a11=.1
  a12=.1
  a21=.1
  a22=.1
  
  #2-species annual plant model----------------------------------------------
    updateN = function(r_arg, Nself, N1, a_intra, a1){
      newN = (r_arg * Nself) /(1 + a_intra * Nself + a1 * N1 )
      newN=rpois(1,newN)         #demographic stochasticity
      return(newN)
                  }
    
    # population size vectors
N=initN
N2=initN2
for(t in 2:totT){
  N[t]=updateN(r1,N[t-1],N2[t-1],a11,a12)
  N2[t]=updateN(r2,N2[t-1],N[t-1],a22,a21)
  }


# do stats -------------------------------------------------------------------

# first, cut off burn in phase
N=N[(totT/10):totT]
lagN=N[1:length(N)-1]
N2=N2[(totT/10):totT]
lagN2=N2[1:length(N2)-1]

# add noise
#obsN=N+rnorm(length(N),0,10)    # normal variation around true N
obsN=exp(log(N)+rnorm(length(N),0,0.2))    # lognormal variation around true N
lagObsN=obsN[1:length(obsN)-1]

#obsN2=N2+rnorm(length(N2),0,10)    # normal variation around true N
obsN2=exp(log(N2)+rnorm(length(N2),0,0.5))    # lognormal variation around true N
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
  
#model true data
print(summary(lm(r.true~relN)))
# model observed data
print(summary(lm(r.obs~relN.obs)))

par(mfrow=c(1,2))
plot(relN,r.true,main="True N")
plot(relN.obs,r.obs,main="Obs N")

# model true data
print(summary(lm(r.true2~relN2)))
# model observed data
print(summary(lm(r.obs2~relN2.obs)))

par(mfrow=c(1,2))
plot(relN2,r.true2,main="True N2")
plot(relN2.obs,r.obs2,main="Obs N2")
  
library("MCMCpack")  
#Bayesian model to account for uncertainty
  summary(MCMCregress(r.true~relN, burnin = 1000, mcmc = 10000,
                      thin = 1, verbose = 0, seed = NA, beta.start = 0, 
                      b0 = 0, B0 = 10, c0 = 5, d0 = 5,
                      marginal.likelihood = "none"))
  
  summary(MCMCregress(r.obs~relN.obs, burnin = 1000, mcmc = 10000,
                      thin = 1, verbose = 0, seed = NA, beta.start = 0, 
                      b0 = 0, B0 = 10, c0 = 5, d0 = 5,
                      marginal.likelihood = "none"))
  
  #Bayesian model to account for uncertainty
  summary(MCMCregress(r.true2~relN2, burnin = 1000, mcmc = 10000,
                      thin = 1, verbose = 0, seed = NA, beta.start = 0, 
                      b0 = 0, B0 = 2, c0 = 5, d0 = 5,
                      marginal.likelihood = "none"))
  
  summary(MCMCregress(r.obs2~relN2.obs, burnin = 1000, mcmc = 10000,
                      thin = 1, verbose = 0, seed = NA, beta.start = 0, 
                      b0 = 0, B0 = 2, c0 = 5, d0 = 5,
                      marginal.likelihood = "none"))
  
  #Bayesian model to account for uncertainty
  real1=as.data.frame(cbind(r.true,relN,rand=rep(0,length(r.true)),group=rep(1,length(r.true))))
  
  summary(MCMChregress(fixed=r.true~relN, random= ~rand, data=real1, group = "group", 
                       burnin = 1000, mcmc = 10000, thin = 10, verbose = 0, seed = NA, beta.start = 0, sigma2.start = NA,
                       mubeta = 0, Vbeta=1.0E6, nu = 5, delta = 5, r = 2, R = diag(1,0.001))$mcmc)
  
  data1=as.data.frame(cbind(r.obs,relN.obs,rand=rep(0,length(r.true)),group=rep(1,length(r.true))))
  
  summary(MCMChregress(fixed=r.obs~relN.obs, random= ~rand, data=data1, group = "group", 
                       burnin = 1000, mcmc = 10000, thin = 10, verbose = 0, seed = NA, beta.start = 0, sigma2.start = NA,
                       mubeta = 0, Vbeta=1.0E6, nu = 5, delta = 5, r = 2, R = diag(1,0.001))$mcmc)
  
  
  
  #Bayesian model to account for uncertainty
  real2=as.data.frame(cbind(r.true2,relN2,rand=rep(0,length(r.true2)),group=rep(1,length(r.true2))))
  
  summary(MCMChregress(fixed=r.true2~relN2, random= ~rand, data=real2, group = "group", 
                       burnin = 1000, mcmc = 10000, thin = 10, verbose = 0, seed = NA, beta.start = 0, sigma2.start = NA,
                       mubeta = 0, Vbeta=1.0E6, nu = 5, delta = 5, r = 2, R = diag(1,0.001))$mcmc)
  
  data2=as.data.frame(cbind(r.obs2,relN2.obs,rand=rep(0,length(r.true2)),group=rep(1,length(r.true2))))
  
  summary(MCMChregress(fixed=r.obs2~relN2.obs, random= ~rand, data=data2, group = "group", 
                       burnin = 1000, mcmc = 10000, thin = 10, verbose = 0, seed = NA, beta.start = 0, sigma2.start = NA,
                       mubeta = 0, Vbeta=10, nu = 5, delta = 5, r = 2, R = diag(1,0.001))$mcmc)
  
