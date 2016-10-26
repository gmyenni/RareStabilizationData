relabunds=c(0.1,0.9) 
t=20 
r=500
a=c(0.1,0.1)
aij=0.1
C=data.frame(Year=1:(t+1),Site=scenarioname,Total=rep(NA,t+1),N1=rep(NA,t+1),N2=rep(NA,t+1))
C[1,-(1:3)]=10000*relabunds
C$Total[1]=sum(C[1,-(1:3)])

#Chesson LV

for(i in 1:t){ 
  tmp1=ifelse(C$N1[i]*r*(1-a[1]*C$N1[i]-aij*(C$Total[i]-C$N1[i]))+C$N1[i]>0,C$N1[i]*r*(1-a[1]*C$N1[i]-aij*(C$Total[i]-C$N1[i]))+C$N1[i],0)
    C$N1[i+1]=rpois(1,tmp1)
  tmp2=ifelse(C$N2[i]*r*(1-a[2]*C$N2[i]-aij*(C$Total[i]-C$N2[i]))+C$N2[i]>0,C$N2[i]*r*(1-a[2]*C$N2[i]-aij*(C$Total[i]-C$N2[i]))+C$N2[i],0)
    C$N2[i+1]=rpois(1,tmp2)
  C$Total[i+1]=sum(C[i+1,-(1:3)],na.rm=T)
}


#Ricker

for(i in 1:t){ 
  tmp1=C$N1[i]*exp(r-a[1]*C$N1[i]-aij*(C$Total[i]-C$N1[i]))
  tmp1=ifelse(tmp1>0,tmp1,0)
  C$N1[i+1]=rpois(1,tmp1)
  tmp2=C$N2[i]*exp(r-a[2]*C$N2[i]-aij*(C$Total[i]-C$N2[i]))
  tmp2=ifelse(tmp2>0,tmp2,0)
  C$N2[i+1]=rpois(1,tmp2)
  C$Total[i+1]=sum(C[i+1,-(1:3)],na.rm=T)
}
