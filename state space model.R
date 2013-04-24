###test the state space model: based on the Gompertz model (log-tranformation)
library(R2WinBUGS)

setwd("C:/Users/gmyenni/Dropbox/projects/niche4neutrality/RareStabilizationData")

# ######################the arbitray data
# year<-seq(1991,2021,1) ##31 years
# N<-length(year)
# yy<-rpois(N,20)
# y<-log(yy)
# y[3]<-NA ##to simulate the missing observations
# y[15]<-NA
# ######################

##using a loop for the case of multiple species
myData<-read.csv("hubbard.txt",sep="\t")
year<-myData$year
N<-max(myData$year)-min(myData$year)+1
yy<-myData[,4]
yy[yy==0]=NA
y<-log(yy) ##log-transformed...

# y<-myData[,c(4:dim(myData)[2])] ##the data for observed abundance

data=list("N","y") 

inits=list()
inits[[1]]=list(r=0.1,b=0.1,sigma1=1,sigma2=2) 
inits[[2]]=list(r=0.1,b=0.1,sigma1=1,sigma2=2)

params=c("r","b","sigma1","sigma2","n_exp")

out=bugs(data,inits,params,model.file="densityD.txt", 
         bugs.directory="C:\\Program Files\\WinBUGS14\\",
         n.chains=2,n.iter=20000,n.burnin=10000,n.thin=50, 
         debug=T,DIC=T)

tmp=grep("n_exp",row.names(out$summary))
estAbun<-as.data.frame(exp(out$summary[tmp,1])) ##estimated abundance

plot(yy-estAbun[,1])

mean(yy-estAbun[,1],na.rm=T)

# ######################################
# ##############################
# model {
#   for(i in 1:(N-1)){
#     y[i+1]~dnegbin(q[i+1],theta)
#     q[i+1]<-theta/(theta+lambda[i+1])
#   }
#   
#   for(i in 1:(N-1)){
#     lambda_mu[i+1]<-lambda[i]*fecundity[i]
#     
#     log(fecundity[i])<-dd*lambda[i]
#     
#     lambda[i+1]~dnegbin(lambda_mu[i+1]/(sigma*sigma),lambda_mu[i+1]/(sigma*sigma-lambda_mu[i+1]))
#   }
#   
#   
#   lambda[1]~dpois(y[1])
#   
#   theta~dgamma(0.001,0.001) 
#   sigma~dgamma(0.001,0.001)
#   dd~dnorm(0,0.0001)
# }


