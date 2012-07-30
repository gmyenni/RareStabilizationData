#This function creates 'rand' number of communities with the same structure as the
#original community (same S, relative abundances, and population variabilities), but 
#with no frequency dependence. It estimates the strength of frequency dependence due
#to uncertainty alone, and fits the expected relationship between relative abundance 
#and strength of frequency dependence and uses this as the 'null expectation' for the
#strength of this relationship.
######################################################################################

null=function(abund) {
  
  library(foreach)
  rand=100
  
######################################################################################
#########################Randomization function using truncated normal################

  library("msm")
  #randomization using truncated normal
  #   randpop=function(vect,rand) {
  #     ave=mean(as.numeric(vect)[which(as.numeric(vect)>=0)]); vari=var(as.numeric(vect)[which(as.numeric(vect)>=0)])
  #     randpop=rtnorm(length(vect)*rand,ave,sqrt(vari),lower=0)
  #     return(randpop)
  #   }
  #   ABUNDR=matrix(NA,rand*dim(abund)[1],dim(abund)[2])
  #   ABUNDR[,4:dim(ABUNDR)[2]]=apply(abund[,4:dim(abund)[2]], 2, randpop, rand)
  
  #randomization using uniform dist
  #     randpop=function(vect,rand) {
  #       randpop=runif(length(vect)*rand,max(min(vect),0),max(vect))
  #       return(randpop)
  #     }
  #     ABUNDR=matrix(NA,rand*dim(abund)[1],dim(abund)[2])
  #     ABUNDR[,4:dim(ABUNDR)[2]]=apply(abund[,4:dim(abund)[2]], 2, randpop, rand)
  #   
  #randomization using shuffling
    ABUNDR1 = foreach(it=1:rand,.combine='rbind') %do% apply(abund,2,sample,replace=F)
    
    
  #fix some columns in randomized matrix
  ABUNDR=matrix(0,dim(ABUNDR1)[1],dim(ABUNDR1)[2])
  for(c in 4:dim(ABUNDR1)[2]) { ABUNDR[,c] =as.numeric(ABUNDR1[,c]) }
  ABUNDR[ABUNDR == -99] <- 0                       #remove -99s
  ABUNDR[,1]=rep(abund[,1],rand)                              #years
  ABUNDR[,2]=rep(abund[,2],rand)                              #sites
  ABUNDR[,3]=rowSums(data.matrix(ABUNDR[,4:dim(ABUNDR)[2]]))  #totals
  
  
######################################################################################
############################Function to fit randomized data###########################  
  randfit=function(iter,ABUNDR,abund) {
    
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
    S=dim(abund)[2]-3
    lastyear=max(abund[,1])
    abundr=ABUNDR[(1+dim(abund)[1]*(iter-1)):(iter*dim(abund)[1]),]
  
  #calculate randomized relative abundances and growth rates:
    
    #calc relative abundance
    relAr=abundr[,4:dim(abundr)[2]]/abundr[,3]
    relAr=relAr[which(abundr[,1]!=lastyear),]
    
    #calc lambda
    ratesr=matrix(data=NA,nrow=nrow(abundr)-1,ncol=ncol(abundr)-3)
    for(i in 1:(length(abundr[,1])-1)) {
      for(x in 4:ncol(abundr)) {
        ratesr[i,x-3]=lambda(abundr)
      }
    }
    ratesr=ratesr[which(abundr[,1]!=lastyear),]
    
  #Calculate stabilization and equilibruim RA exactly as done with the real data
    null_intercept=matrix(NA,S)
    null_slope=matrix(NA,S)
  for(s in 1:S) {
                               
      null_intercept[s]=ifelse(sum( !is.na(ratesr[,s]))==0,NA,-lm(ratesr[,s]~relAr[,s])$coefficients[1]/lm(ratesr[,s]~relAr[,s])$coefficients[2])
      null_slope[s]=ifelse(sum( !is.na(ratesr[,s]))==0,NA,lm(ratesr[,s]~relAr[,s])$coefficients[2]) }
      results=data.frame(null_intercept,null_slope)
      results1=results[which(results$null_slope<=0),]  
      results2=results1[which(results1$null_intercept<=1),]  
      results2=results2[which(results2$null_intercept>=0),]
    
  #returns the strength of the pattern in the randomized data 
  pattern=ifelse(sum(!is.na(results2$null_slope))==0,NA,cor(as.numeric(results2$null_intercept),-as.numeric(results2$null_slope), method = "kendall", use = "complete.obs"))

    return(pattern)
}
######################################################################################  

null_pattern =  foreach(iter=1:rand, .combine=rbind) %do% randfit(iter,ABUNDR,abund)
  
}