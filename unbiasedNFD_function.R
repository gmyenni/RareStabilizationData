##Function to use corrected estimates of population density to estimate NFD and EF

unbiasedNFD=function(dataset) {
#Load Gompertz functions
source("./GompertzFuns.r")
source("./findv_function.R")

  #Get unbiased abundance estimates for a population time series
  fitN=function(y) {
    fit_out=fitGompertzSS(y)  #fit Gompertz
    v=findv(y,fit_out)$v      #use estimates
    N=exp(y-v)                #correct N
    
    return(N)
    
      }

#Fix all abundance estimates and re-assemble community matrix
dataset[dataset<0]=0
for(i in 4:dim(dataset)[2]){

  newN=fitN(log(as.numeric(dataset[,i]+0.000001)))-0.000001
  newN[newN<0]=0
  dataset[,i]=newN
  
  }
  dataset[,3]=apply(dataset[,4:dim(dataset)[2]],1,sum,na.rm=T)
return(dataset)
}
