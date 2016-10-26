#Growth function to produce 10-species abundances based on Chesson per capita Lotka Volterra 
#(a is vector of intraspecific alphas), with demographic stochasticity

growth=function(scenarioname,a) {
  
  relabunds=c(0.001,0.01,0.02,0.04,0.08,0.09,0.149,0.16,0.18,0.27) 
  t=200
  r=1.5
  aij=rep(0.001,9)
  C=data.frame(Year=1:(t+1),Site=scenarioname,Total=rep(0,t+1),N1=rep(0,t+1),N2=rep(0,t+1),
               N3=rep(0,t+1),N4=rep(0,t+1),N5=rep(0,t+1),N6=rep(0,t+1),N7=rep(0,t+1),
               N8=rep(0,t+1),N9=rep(0,t+1),N10=rep(0,t+1))
  C[1,-(1:3)]=1000*relabunds
  C$Total[1]=sum(C[1,-(1:3)])
  
  for(i in 1:t){ 
    C$N1[i+1]=rpois(1,C$N1[i]*r*(1-a[1]*C$N1[i]-sum(C[i,-c(1:3,4)]*aij))+C$N1[i])
    C$N2[i+1]=rpois(1,C$N2[i]*r*(1-a[2]*C$N2[i]-sum(C[i,-c(1:3,5)]*aij))+C$N2[i])
    C$N3[i+1]=rpois(1,C$N3[i]*r*(1-a[3]*C$N3[i]-sum(C[i,-c(1:3,6)]*aij))+C$N3[i])
    C$N4[i+1]=rpois(1,C$N4[i]*r*(1-a[4]*C$N4[i]-sum(C[i,-c(1:3,7)]*aij))+C$N4[i])
    C$N5[i+1]=rpois(1,C$N5[i]*r*(1-a[5]*C$N5[i]-sum(C[i,-c(1:3,8)]*aij))+C$N5[i])
    C$N6[i+1]=rpois(1,C$N6[i]*r*(1-a[6]*C$N6[i]-sum(C[i,-c(1:3,9)]*aij))+C$N6[i])
    C$N7[i+1]=rpois(1,C$N7[i]*r*(1-a[7]*C$N7[i]-sum(C[i,-c(1:3,10)]*aij))+C$N7[i])
    C$N8[i+1]=rpois(1,C$N8[i]*r*(1-a[8]*C$N8[i]-sum(C[i,-c(1:3,11)]*aij))+C$N8[i])
    C$N9[i+1]=rpois(1,C$N9[i]*r*(1-a[9]*C$N9[i]-sum(C[i,-c(1:3,12)]*aij))+C$N9[i])
    C$N10[i+1]=rpois(1,C$N10[i]*r*(1-a[10]*C$N10[i]-sum(C[i,-c(1:3,13)]*aij))+C$N10[i])
    C$Total[i+1]=sum(C[i+1,-(1:3)],na.rm=T)
  }
  
  return(C)
  
}