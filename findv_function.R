findv = function(y,fit) {            
  a=fit$a
  c=fit$c
  sp2=fit$sp2
  so2=fit$so2
  a1=y[1]
  P1=10
  F=P1+so2
  v=NA
  v[1]=y[1]-a1
  ll=log(dnorm(v[1],mean=0,sd=sqrt(F)))
  for (i in 2:length(y)) {
    a1=a+c*a1+c*P1/F*v[i-1]
    P1=c^2*P1*(1-P1/F*is.finite(y[i-1]))+sp2
    F=P1+so2

    if (is.finite(y[i])) {
      v[i]=y[i]-a1
      ll=ll+log(dnorm(v[i],mean=0,sd=sqrt(F)))
    }                                               
  }
  #if(!is.finite(ll)) ll=-1e300    
  list(ll=-ll,a1=a1,v=v)
}