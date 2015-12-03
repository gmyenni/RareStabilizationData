#####Use stabil function results to plot estimates of NFD with raw data, overall pattern, and p value

rawfigures=function(relA,rates,sp_names,results2,pattern,null_pattern,p.val,dataname) {
#########################Figures#######################################
layout(matrix(c(1,2,3),1,3))
S=length(sp_names)
color=c("springgreen4","royalblue3","tomato","tomato4","yellowgreen","purple4","springgreen","yellow2","thistle4"
        ,"tan2","violetred3","turquoise1","rosybrown3","palevioletred4","tan4","violet","steelblue4","palevioletred3"
        ,"thistle2","red4","seagreen4","rosybrown4","paleturquoise4","palegreen4","darksalmon",1:50)
#library("RColorBrewer")
symbol=c(1:25,32:127)

#plot data and fitted negative frequency dependence
plot(NA,NA,xlim=c(0,1),ylim=range(rates,na.rm=T),xlab="Relative Abundance",ylab=expression( paste('Growth Rate (log  ', lambda,')')),main=dataname )
abline(h=0,col="grey",lwd=3)

for(i in 1:S){
  points(relA[,i],rates[,i],pch=symbol[i],col=color[i],cex=0.6)
  if(sum(is.finite(rates[,i]))>1) {
  abline(lm(rates[,i]~relA[,i],na.action=na.omit),col=color[i],lwd=2.5) }}

# #nonlinear
# plot(NA,NA,xlim=c(-5,0),ylim=range(rates,na.rm=T),xlab="log(Relative Abundance)",ylab=expression( paste('Growth Rate (log  ', lambda,')')),main=dataname )
# abline(h=0,col="grey",lwd=3)
# 
# for(i in 1:S){
#   points(log(relA[,i]),rates[,i],pch=symbol[i],col=color[i],cex=0.6)
#   if(sum(is.finite(rates[,i]))>1) {
#     abline(lm(rates[,i]~log(relA[,i]),na.action=na.omit),col=color[i],lwd=2.5) }}

#legend("topright",legend=sp_names,col=color[1:S],pch=symbol[1:S],lwd=2,cex=.75,bty='n')

#plot relationship between frequency and strength of stabilization
plot(log(results2$intercept),log(-as.numeric(results2$slope)),pch=19,bty='l',xlab="log(Equilibrium Frequency)",ylab='log(Strength of NFD)',main="Observed Assymetry",col=color)
abline(lm(log(-as.numeric(results2$slope))~log(as.numeric(results2$intercept))),lwd=2,col='red')
mtext(side=3,paste('pattern=',round(pattern,3),sep=" "),adj=0,line=0.5,cex=1)
#legend("topright",legend=sp_names,col=color[1:S],pch=19,cex=.75,bty='n')
#identify outliers
#identify(log(as.numeric(results2[,2])),log(-as.numeric(results2[,3])),results2$sp,xpd=NA,cex=0.5)

# add a new layer to draw arrow showing direction of stabilization
# parsave=par(new = TRUE,  mar = c(0,0,0,0))
# plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxt='n',yaxt='n',bty='n',col='white') 
# arrows(0.03,0.2,0.03,0.85,2,length=.1,angle=30,lwd=2,xpd=NA)
# text(0.04, 0.87,'STRONG',xpd=NA,col='red')
# text(0.05, 0.18,'WEAK',xpd=NA,col='red')
# par(parsave)

#plot null pattern
hist(null_pattern,breaks=20,xlim=c(min(min(null_pattern,pattern,na.rm=T)),max(max(null_pattern),pattern+.2,na.rm=T)),main="Histogram of null pattern")
abline(v=pattern,col='red',lwd=2)
mtext(side=3,paste('p-val=',round(p.val,3),sep=" "),adj=0,line=0.5,cex=0.6)
}