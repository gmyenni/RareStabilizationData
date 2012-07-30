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
results=data.frame(sp=NA,intercept=0,slope=0)
for(i in 1:S){
if(sum(rates[,i],na.rm=T)==0){
results[i,]=c(sp_names[i],NA,NA)
}else{                           
results[i,]=c(sp_names[i],-lm(rates[,i]~relA[,i])$coefficients[1]/lm(rates[,i]~relA[,i])$coefficients[2],lm(rates[,i]~relA[,i])$coefficients[2]) }
}

results1=results[which(as.numeric(results$slope)<=0),]
results2=results1[which(as.numeric(results1$intercept)<=1),]
results2=results2[which(as.numeric(results2$intercept)>=0),]

#Fit relationship between frequency and strength of stabilization (inverse transformed) 
if(is.null(results2)==F) {
pattern=summary(lm(log(-as.numeric(results2$slope))~log(as.numeric(results2$intercept))))
}

#Compare to randomized data
source("./null_function.R")
null_pattern=null(abund)
null.mean=mean(null_pattern,na.rm=T)
p.val=length(which(null_pattern<pattern$coefficients[2,1]))/length(null_pattern)

#source('./rawfigures.R')

#save results
  parcel=list(results=results,pattern=cbind(S1=S,S2=dim(results2)[1],T=lastyear-firstyear+1,pattern=pattern$coefficients[2,1],null.mean=null.mean,p.val=p.val))
  return(parcel)

}