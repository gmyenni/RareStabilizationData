#Produces strength of stabilization figures and calculates estimates from species abundance matrix and energy matrix
##with column order: Year, Site, Total, SP1, SP2, . . . (sorted by Site then by Year)

stabil=function(abund, sp_names, dataname) {

#calculate observed growth rates:

#log lambda function
lambda=function(file) { r=log(file[i+1,x]/file[i,x])
return(r)
}

#data:
abund[abund<0]=NA
abund[,3]=apply(abund[,-(1:3)],1,sum,na.rm=T)
S=length(sp_names)
lastyear=max(abund[,1])
firstyear=min(abund[,1])

#calc relative abundance
relA=abund[,4:dim(abund)[2]]/abund[,3]
relA=as.matrix(relA[which(abund[,1]!=lastyear),])
relA[which(!is.finite(relA))] = NA

#calc lambda
rates=matrix(data=NA,nrow=nrow(abund)-1,ncol=ncol(abund)-3)
for(i in 1:(length(abund[,1])-1)) {
  for(x in 4:ncol(abund)) {
    rates[i,x-3]=lambda(abund)
  }
}
rates=rates[which(abund[,1]!=lastyear),]
rates[which(!is.finite(rates))] = NA

#Calculate equilibrium frequency and stabilization
results=data.frame(sp=NA,intercept=0,slope=0,persist=NA,mean=NA,median=NA)
for(i in 1:S){
  
  results[i,1]=sp_names[i]
  
  results[i,4]=length(which(abund[,(i+3)]>0))/length(abund[,(i+3)])
  
  results[i,5]=mean(abund[which(abund[,(i+3)]>=0),(i+3)],na.rm=T)
  
  results[i,6]=median(abund[which(abund[,(i+3)]>=0),(i+3)],na.rm=T)
  
if(sum(rates[,i],na.rm=T)==0){
results[i,2:3]=c(NA,NA)
}else{                           

  model=lm(rates[,i]~relA[,i])         #linear
  results[i,2:3]=c(-model$coefficients[1]/model$coefficients[2],model$coefficients[2]) }
  
  #model=lm(rates[,i]~log(relA[,i]))     #non-linear
  #results[i,2:3]=c(exp(-model$coefficients[1]/model$coefficients[2]),model$coefficients[2]) }
  
}

results1=results[which(as.numeric(results$slope)<=0),]
results2=results1[which(as.numeric(results1$intercept)<=1),]
results2=results2[which(as.numeric(results2$intercept)>=0),]

#Fit relationship between frequency and strength of stabilization (inverse transformed) 
if(is.null(results2)==F) {
pattern=cov(log(as.numeric(results2$intercept)),log(-as.numeric(results2$slope)),use="complete.obs")

}

#Compare to randomized data
source("./null_function.R")
null_pattern=null(abund)
null.mean=mean(null_pattern,na.rm=T)
p.val=length(which(null_pattern<pattern))/length(null_pattern)

#source rawfigures function to plot all results
source('./rawfigures.R')
rawfigures(relA,rates,sp_names,results2,pattern,null_pattern,p.val,dataname)

#save results
  parcel=list(results=results,pattern=cbind(S1=S,S2=dim(results2)[1],T=lastyear-firstyear+1,pattern=pattern,null.mean=null.mean,p.val=p.val))
  return(parcel)

}