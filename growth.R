#Growth function to produce 10-species abundances based entirely on frequency-dependent 
#growth (arguments control strength of NFD for each species), with demographic stochasticity

growth=function(scenarioname,a,b) {

  relabunds=c(0.001,0.01,0.02,0.04,0.08,0.09,0.149,0.16,0.18,0.27) 
t=20  
C=data.frame(Year=1:(t+1),Site=scenarioname,Total=0,N1=rep(0,t+1),N2=rep(0,t+1),
             N3=rep(0,t+1),N4=rep(0,t+1),N5=rep(0,t+1),N6=rep(0,t+1),N7=rep(0,t+1),
             N8=rep(0,t+1),N9=rep(0,t+1),N10=rep(0,t+1))
C[1,-(1:3)]=1000*relabunds
C$Total[1]=sum(C[1,-(1:3)])

  for(i in 1:t){ 
    C$N1[i+1]=rpois(1,exp(b[1]*(C$N1[i]/C$Total[i])+a[1])*C$N1[i])
    C$N2[i+1]=rpois(1,exp(b[2]*(C$N2[i]/C$Total[i])+a[2])*C$N2[i])
    C$N3[i+1]=rpois(1,exp(b[3]*(C$N3[i]/C$Total[i])+a[3])*C$N3[i])
    C$N4[i+1]=rpois(1,exp(b[4]*(C$N4[i]/C$Total[i])+a[4])*C$N4[i])
    C$N5[i+1]=rpois(1,exp(b[5]*(C$N5[i]/C$Total[i])+a[5])*C$N5[i])
    C$N6[i+1]=rpois(1,exp(b[6]*(C$N6[i]/C$Total[i])+a[6])*C$N6[i])
    C$N7[i+1]=rpois(1,exp(b[7]*(C$N7[i]/C$Total[i])+a[7])*C$N7[i])
    C$N8[i+1]=rpois(1,exp(b[8]*(C$N8[i]/C$Total[i])+a[8])*C$N8[i])
    C$N9[i+1]=rpois(1,exp(b[9]*(C$N9[i]/C$Total[i])+a[9])*C$N9[i])
    C$N10[i+1]=rpois(1,exp(b[10]*(C$N10[i]/C$Total[i])+a[10])*C$N10[i])
    C$Total[i+1]=sum(C[i+1,-(1:3)],na.rm=T)
  }
  
return(C)

}