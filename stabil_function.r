#Produces strength of stabilization figures and calculates estimates from species abundance matrix and energy matrix
##with column order: Year, Site, Total, SP1, SP2, . . . (sorted by Site then by Year)

stabil=function(abund, sp_names, dataname) {

#calculate observed growth rates:

#log lambda function
lambda=function(file) {
if(file[i+1,x]<=0 | file[i,x]<=0){
  r=NA
}else{
  r=log(file[i+1,x]/file[i,x])
}
return(r)
}

#data:
S=length(sp_names)
lastyear=max(abund[,1])
firstyear=min(abund[,1])

#calc relative abundance
relA=abund[,4:dim(abund)[2]]/abund[,3]
relA=relA[which(abund[,1]!=lastyear),]


#calc lambda
rates=matrix(data=NA,nrow=nrow(abund)-1,ncol=ncol(abund)-3)
for(i in 1:(length(abund[,1])-1)) {
  for(x in 4:ncol(abund)) {
    rates[i,x-3]=lambda(abund)
  }
}
rates=rates[which(abund[,1]!=lastyear),]

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
results[i,2:3]=c(-lm(rates[,i]~relA[,i])$coefficients[1]/lm(rates[,i]~relA[,i])$coefficients[2],lm(rates[,i]~relA[,i])$coefficients[2]) }
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