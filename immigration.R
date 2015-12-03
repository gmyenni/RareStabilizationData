#Function to produce 2-species abundances based entirely on frequency-dependent 
#growth (arguments control strength of NFD for each species), with demographic stochasticity
#and frequency-dependent immigration

immigration=function(scenarioname,a,b) {
 
#Focal Community 
  relabunds=c(0.001,0.01,0.02,0.04,0.08,0.09,0.149,0.16,0.18,0.27)  
  t=20  
  C=data.frame(Year=1:(t+1),Site=scenarioname,Total=0,N1=rep(0,t+1),N2=rep(0,t+1),N3=rep(0,t+1),N4=rep(0,t+1),
               N5=rep(0,t+1),N6=rep(0,t+1),N7=rep(0,t+1),N8=rep(0,t+1),N9=rep(0,t+1),N10=rep(0,t+1))
  C[1,-(1:3)]=1000*relabunds
  C$Total[1]=sum(C[1,-(1:3)])
 
#Source community 
  relabunds2=rev(relabunds); b2=rev(b); a2=-b2*relabunds2 
  C2=data.frame(Year=1:(t+1),Site=scenarioname,Total=0,N1=rep(0,t+1),N2=rep(0,t+1),N3=rep(0,t+1),N4=rep(0,t+1),
                N5=rep(0,t+1),N6=rep(0,t+1),N7=rep(0,t+1),N8=rep(0,t+1),N9=rep(0,t+1),N10=rep(0,t+1))
  C2[1,-(1:3)]=1000*relabunds2
  C2$Total[1]=sum(C2[1,-(1:3)])
  
#Grow both communities simultaneously, with immigration and emmigration
  for(i in 1:t){ 
    #Community 1 frequency-dependent growth
    CN1temp=rpois(1,exp(b[1]*(C$N1[i]/C$Total[i])+a[1])*C$N1[i])
    CN2temp=rpois(1,exp(b[2]*(C$N2[i]/C$Total[i])+a[2])*C$N2[i])
    CN3temp=rpois(1,exp(b[3]*(C$N3[i]/C$Total[i])+a[3])*C$N3[i])
    CN4temp=rpois(1,exp(b[4]*(C$N4[i]/C$Total[i])+a[4])*C$N4[i])
    CN5temp=rpois(1,exp(b[5]*(C$N5[i]/C$Total[i])+a[5])*C$N5[i])
    CN6temp=rpois(1,exp(b[6]*(C$N6[i]/C$Total[i])+a[6])*C$N6[i])
    CN7temp=rpois(1,exp(b[7]*(C$N7[i]/C$Total[i])+a[7])*C$N7[i])
    CN8temp=rpois(1,exp(b[8]*(C$N8[i]/C$Total[i])+a[8])*C$N8[i])
    CN9temp=rpois(1,exp(b[9]*(C$N9[i]/C$Total[i])+a[9])*C$N9[i])
    CN10temp=rpois(1,exp(b[10]*(C$N10[i]/C$Total[i])+a[10])*C$N10[i])
    
    #Community 2 frequency-dependent growth
    C2N1temp=rpois(1,exp(b2[1]*(C2$N1[i]/C2$Total[i])+a2[1])*C2$N1[i])
    C2N2temp=rpois(1,exp(b2[2]*(C2$N2[i]/C2$Total[i])+a2[2])*C2$N2[i])
    C2N3temp=rpois(1,exp(b2[3]*(C2$N3[i]/C2$Total[i])+a2[3])*C2$N3[i])
    C2N4temp=rpois(1,exp(b2[4]*(C2$N4[i]/C2$Total[i])+a2[4])*C2$N4[i])
    C2N5temp=rpois(1,exp(b2[5]*(C2$N5[i]/C2$Total[i])+a2[5])*C2$N5[i])
    C2N6temp=rpois(1,exp(b2[6]*(C2$N6[i]/C2$Total[i])+a2[6])*C2$N6[i])
    C2N7temp=rpois(1,exp(b2[7]*(C2$N7[i]/C2$Total[i])+a2[7])*C2$N7[i])
    C2N8temp=rpois(1,exp(b2[8]*(C2$N8[i]/C2$Total[i])+a2[8])*C2$N8[i])
    C2N9temp=rpois(1,exp(b2[9]*(C2$N9[i]/C2$Total[i])+a2[9])*C2$N9[i])
    C2N10temp=rpois(1,exp(b2[10]*(C2$N10[i]/C2$Total[i])+a2[10])*C2$N10[i])
    
    #Community 1 10% immigration/emmigration
    C$N1[i+1]=0.9*CN1temp + 0.1*C2N1temp
    C$N2[i+1]=0.9*CN2temp + 0.1*C2N2temp
    C$N3[i+1]=0.9*CN3temp + 0.1*C2N3temp
    C$N4[i+1]=0.9*CN4temp + 0.1*C2N4temp
    C$N5[i+1]=0.9*CN5temp + 0.1*C2N5temp
    C$N6[i+1]=0.9*CN6temp + 0.1*C2N6temp
    C$N7[i+1]=0.9*CN7temp + 0.1*C2N7temp
    C$N8[i+1]=0.9*CN8temp + 0.1*C2N8temp
    C$N9[i+1]=0.9*CN9temp + 0.1*C2N9temp
    C$N10[i+1]=0.9*CN10temp + 0.1*C2N10temp
    
    #Community 2 10% immigration/emmigration
    C2$N1[i+1]=0.9*C2N1temp + 0.1*CN1temp
    C2$N2[i+1]=0.9*C2N2temp + 0.1*CN2temp
    C2$N3[i+1]=0.9*C2N3temp + 0.1*CN3temp
    C2$N4[i+1]=0.9*C2N4temp + 0.1*CN4temp
    C2$N5[i+1]=0.9*C2N5temp + 0.1*CN5temp
    C2$N6[i+1]=0.9*C2N6temp + 0.1*CN6temp
    C2$N7[i+1]=0.9*C2N7temp + 0.1*CN7temp
    C2$N8[i+1]=0.9*C2N8temp + 0.1*CN8temp
    C2$N9[i+1]=0.9*C2N9temp + 0.1*CN9temp
    C2$N10[i+1]=0.9*C2N10temp + 0.1*CN10temp

  C$Total[i+1]=sum(C[i+1,-(1:3)],na.rm=T)
  C2$Total[i+1]=sum(C2[i+1,-(1:3)],na.rm=T)
  }
  
  return(C)
  
}