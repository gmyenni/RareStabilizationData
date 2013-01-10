# Functions for fitting a Gompertz state space model under maximum likelihood.
# Written by Jonas Knape (jknape@erkeley.edu).


# Provides negative log likelihood for use by optim.
gompertzSSnLL = function(pOpt,y,a1,P1,iFix=rep(FALSE,4),pFix=NA) {            
  if (length(pOpt) + length(pFix) != 4 | sum(iFix)!= length(pFix)) stop("Number of parameters to estimate not correctly specified.")
  p=rep(0,4)
  p[!iFix]=pOpt
  p[iFix]=pFix
  a=p[1]
  c=p[2]
  sp2=exp(p[3])
  so2=exp(p[4])
  F=P1+so2
  v=y[1]-a1
  ll=log(dnorm(v,mean=0,sd=sqrt(F)))
  for (i in 2:length(y)) {
    a1=a+c*a1+c*P1/F*v
    P1=c^2*P1*(1-P1/F*is.finite(y[i-1]))+sp2
    F=P1+so2
    v=0
    if (is.finite(y[i])) {
      v=y[i]-a1
      ll=ll+log(dnorm(v,mean=0,sd=sqrt(F)))
    }                                               
  }
  if(!is.finite(ll)) ll=-1e300    
  -ll
}

                                                              
# Maximum likelihood estimation for the model
# y[i]~N(x[i],so2)
# x[i]~N(a+c*x[i-1],sp2)
# x[1]~N(a1,P1)
# nStart specifies the number of random starting values
# for the BFGS algorithm. 
fitGompertzSS = function(y,a1=y[1],P1=10,nStart=50,a=NA,c=NA,sp2=NA,so2=NA) {
  # Use the sample mean and variance of the time series to guide
  # choice of optimization starting values.
  mu=mean(y,na.rm=TRUE)
  s2=var(y,na.rm=TRUE)
  if (is.na(c)) c0=runif(nStart,-1.1,1.1) else c0=c
  if (is.na(a)) a0=rnorm(nStart,mean=mu*(abs(c0)<1)*(1-c0),sd=sqrt(s2)/10) else a0=NA
  ds2=runif(nStart,.5,1.5)
  if (is.na(sp2) & is.na(so2)) {
   ap0=runif(nStart,0,1)
   lsp20=log(s2*ds2*ap0)
   lso20=log(s2*ds2*(1-ap0))
  }
  if (!is.na(sp2) & is.na(so2)) {
   lso20=log((s2*ds2-sp2)*(s2*ds2>sp2) + (s2*ds2<sp2)*exp(runif(nStart,0,.5)))
   lsp20=NA
  }
  if (is.na(sp2) & !is.na(so2)) {
     lsp20=log((s2*ds2-so2)*(s2*ds2>so2) + (s2*ds2<so2)*exp(runif(nStart,0,.5)))
     lso20=NA                                                                     
  }
  # Specify which parameters to hold fixed during optimization
  iFix=!is.na(c(a,c,sp2,so2))
  pFix=c(a,c,log(sp2),log(so2))[iFix]
  p=cbind(a=a0,c=c0,lsp2=lsp20,lso2=lso20)[,!iFix,drop=FALSE]
  fit=optim(p[1,],gompertzSSnLL,method="BFGS",y=y,a1=a1,P1=P1,pFix=pFix,iFix=iFix,control=list(maxit=2000))
  for (i in seq(from=2,length.out=nStart-1)) {
    tmp=optim(p[i,],gompertzSSnLL,method="BFGS",y=y,a1=a1,P1=P1,pFix=pFix,iFix=iFix,control=list(maxit=2000))
    if (tmp$value < fit$value) {
       fit=tmp
    }
  }
  list(a=fit$par["a"],c=fit$par["c"],sp2=exp(fit$par["lsp2"]),so2=exp(fit$par["lso2"]),LL=-fit$value,convergence=fit$convergence,message=fit$message)
}

