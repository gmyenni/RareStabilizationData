#This function collects one random realization from the randomization method 
#that maintains the same structure as the
#original community (same S, relative abundances, and population variabilities), but 
#with no frequency dependence
######################################################################################

background=function(abund) {
  
  ######################################################################################
  #########################Randomization function using truncated normal################
  
  #randomization using truncated normal
  #   randpop=function(vect,rand) {
  #     ave=mean(as.numeric(vect)[which(as.numeric(vect)>=0)]); vari=var(as.numeric(vect)[which(as.numeric(vect)>=0)])
  #     randpop=rtnorm(length(vect)*rand,ave,sqrt(vari),lower=0)
  #     return(randpop)
  #   }
  #   ABUNDR=matrix(NA,rand*dim(abund)[1],dim(abund)[2])
  #   ABUNDR[,4:dim(ABUNDR)[2]]=apply(abund[,4:dim(abund)[2]], 2, randpop, rand)
  
  #   
  #randomization using shuffling
  ABUNDR1 = data.frame(apply(abund,2,sample,replace=F))
  
  
  #fix some columns in randomized matrix
  ABUNDR1[, 4:dim(ABUNDR1)[2]] <- sapply(ABUNDR1[, 4:dim(ABUNDR1)[2]], function(f){as.numeric(f)})
  ABUNDR1[ABUNDR1 == -99] <- 0                                    #remove -99s
  ABUNDR1[,1]=abund[,1]                                           #years
  ABUNDR1[,2]=abund[,2]                                           #sites
  ABUNDR1[,3]=rowSums(data.matrix(ABUNDR1[,4:dim(ABUNDR1)[2]]))   #totals
  
  
  ######################################################################################
  ############################Fit randomized data###########################  
    
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
    
    #calculate randomized relative abundances and growth rates:
    
    #calc relative abundance
    relAr=ABUNDR1[,4:dim(ABUNDR1)[2]]/ABUNDR1[,3]
    relAr=relAr[which(ABUNDR1[,1]!=lastyear),]
    
    #calc lambda
    ratesr=matrix(data=NA,nrow=nrow(ABUNDR1)-1,ncol=ncol(ABUNDR1)-3)
    for(i in 1:(length(ABUNDR1[,1])-1)) {
      for(x in 4:ncol(ABUNDR1)) {
        ratesr[i,x-3]=lambda(ABUNDR1)
      }
    }
    ratesr=ratesr[which(ABUNDR1[,1]!=lastyear),]
    
    #Calculate stabilization and equilibruim RA exactly as done with the real data
    null_intercept=matrix(NA,S)
    null_slope=matrix(NA,S)
    
    if(is.null(ratesr)==F) { 
      if(sum(!is.na(ratesr))!=0) {  #only do analysis if randomization resulted in values
        for(s in 1:S) {
          if(sum(ratesr[,s],na.rm=T)!=0) {
            null_intercept[s]=-lm(ratesr[,s]~relAr[,s])$coefficients[1]/lm(ratesr[,s]~relAr[,s])$coefficients[2]
            null_slope[s]=lm(ratesr[,s]~relAr[,s])$coefficients[2]
            
          }}}}
        
        results=data.frame(null_intercept,null_slope)
        #results1=results[which(results$null_slope<=0),]  
        #results2=results1[which(results1$null_intercept<=1),]  
        #results2=results2[which(results2$null_intercept>=0),]
        
return(results)
  
}