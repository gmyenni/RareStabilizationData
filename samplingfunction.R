#Sampling function to create simulated observed abundances from a dataframe of actual
#abundances, using the negative binomial distribution, and controlling for the size parameter

sampling=function(C,size) {

for(i in 1:dim(C)[1]) { 
  C$N1[i]=rnbinom(1,mu=C$N1[i],size=size[1])
  C$N2[i]=rnbinom(1,mu=C$N2[i],size=size[2])
  C$N3[i]=rnbinom(1,mu=C$N3[i],size=size[3])
  C$N4[i]=rnbinom(1,mu=C$N4[i],size=size[4])
  C$N5[i]=rnbinom(1,mu=C$N5[i],size=size[5])
  C$N6[i]=rnbinom(1,mu=C$N6[i],size=size[6])
  C$N7[i]=rnbinom(1,mu=C$N7[i],size=size[7])
  C$N8[i]=rnbinom(1,mu=C$N8[i],size=size[8])
  C$N9[i]=rnbinom(1,mu=C$N9[i],size=size[9])
  C$N10[i]=rnbinom(1,mu=C$N10[i],size=size[10])
  C$Total[i]=sum(C[i,-(1:3)])
}
return(C)
}