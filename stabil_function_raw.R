#Produces strength of stabilization figures and calculates estimates from species abundance matrix and energy matrix
##with column order: Year, Site, Total, SP1, SP2, . . . (sorted by Site then by Year)

stabilRA=function(abund, sp_names, dataname) {

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
results[i,]=c(sp_names[i],mean(relA[,i],na.rm=T),lm(rates[,i]~relA[,i])$coefficients[2]) }
}

results1=results[which(results$slope<=0),]
results2=results1[which(results1$intercept<=1),]
results2=results2[which(results2$intercept>=0),]

#Fit relationship between frequency rank and strength of stabilization
  pattern=cor.test(as.numeric(results2$intercept),-as.numeric(results2$slope), method = "kendall", use = "complete.obs")


#########################Figures#######################################
  color=c("springgreen4","royalblue3","tomato","tomato4","yellowgreen","purple4","springgreen","yellow2","thistle4"
        ,"tan2","violetred3","turquoise1","rosybrown3","palevioletred4","tan4","violet","steelblue4","palevioletred3"
        ,"thistle2","red4","seagreen4","rosybrown4","paleturquoise4","palegreen4","darksalmon",1:50)
#library("RColorBrewer")
  symbol=c(1:25,32:127)
  layout(matrix(c(1,1,2,3), 2, 2, byrow = F),widths=matrix(c(1.5,1),1))

#plot data and fitted negative frequency dependence
  plot(NA,NA,xlim=c(0,1),ylim=range(rates,na.rm=T),main=dataname,xlab="Relative Abundance",ylab=expression( paste('Growth Rate (log  ', lambda,')')) )
  abline(h=0,col="grey",lwd=3)
  
  for(i in 1:S){
  points(relA[,i],rates[,i],pch=symbol[i],col=color[i]) }
  for(i in intersect(which(results[,3]<0),which(results[,2]>0))) {
  abline(lm(rates[,i]~relA[,i]),col=color[i],lwd=2) }
  for(i in intersect(which(results[,3]<0),which(results[,2]<0))) {
  abline(lm(rates[,i]~relA[,i]),col=color[i],lwd=2,lty=2) }
  
  legend("topright",legend=sp_names,col=color[1:S],pch=symbol[1:S],lwd=2,cex=.75,bty='n')
  mtext(side=3,paste('pattern=',round(pattern$estimate,3),'p-val=',round(pattern$p.value,3),sep=" "),adj=0,line=0.5,cex=0.75)

#plot relationship between frequency and strength of stabilization
  plot(as.numeric(results2[,2]),1/-as.numeric(results2[,3]),pch=19,bty='l',xlab="Equilibrium Frequency",ylab=expression(paste('Stabilizatio', n^-1)),main="Observed pattern")
  #identify outliers
  identify(as.numeric(results2[,2]),1/-as.numeric(results2[,3]),results2$sp,xpd=NA,cex=0.5)

# add a new layer to draw arrow showing direction of stabilization
  parsave=par(new = TRUE,  mar = c(0,0,0,0))
  plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxt='n',yaxt='n',bty='n',col='white') 
  arrows(0.03,0.2,0.03,0.85,1,length=.1,angle=30,lwd=2,xpd=NA)
  text(0.04, 0.87,'WEAK',xpd=NA,col='red')
  text(0.05, 0.18,'STRONG',xpd=NA,col='red')
  par(parsave)


#save results
  parcel=list(results=results,pattern=pattern)
  return(parcel)

}