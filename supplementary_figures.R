library('ggplot2')

results=read.csv("./AllResults.csv",header=T)
results$rare[which(results$RA>=0.22)]="C"
results$rare[which(results$RA<0.22)]="R"
results$FD=NA
results$FD[which(results$slope<0)]="NFD"
results$FD[which(results$slope>0)]="PFD"
results$EF=NA
results$EF[which(results$intercept<0)]="EF<0"
results$EF[which(results$intercept>0)]="0<EF<1"
results$EF[which(results$intercept>1)]="EF>1"
results$category=paste(results$EF,results$FD)

pattern=read.csv("./AllPattern.csv",header=T)
pattern$Pcat[which(pattern$p.val<0.1)]="Y"
pattern$Pcat[which(pattern$p.val>0.1)]="N"
pattern$Srat=pattern$Persistent.S/pattern$Observed.S
pattern$Pdiff=pattern$Pattern-pattern$Null.Pattern
sig.list=sort(pattern$Site[which(pattern$Pcat=="Y")])
results$sig="N"
for(i in 1:length(sig.list)) {
  sig.site=sig.list[i]
results$sig[which(results$site==sig.site)]="Y"}

rare=results[which(results$rare=="R"),]
ephemeral=results[which(results$Pcat=="N"),]
persist=results[which(results$Pcat=="P"),]
###little function to count sample size for figures###

give.n <- function(x){
  return(c(y = 0.99, label = length(x)))
}

ggplot(results, aes(factor(FD), RA)) + geom_boxplot() + facet_wrap(~Pcat)

ggplot(rare, aes(factor(FD), RA)) + geom_boxplot() + facet_wrap(~Pcat)

###Plot RAs for categories of EF and FD###
ggplot(results, aes(factor(FD), RA,colour=category)) + geom_boxplot() + facet_wrap(~EF) +
  stat_summary(fun.data = give.n, geom = "text") + theme_bw() + scale_fill_discrete() +
  scale_x_discrete("frequency dependence category") + scale_y_continuous("observed relative abundance") +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
     axis.title.y  = element_text(size=12), axis.title.x  = element_text(size=12),
     axis.text.y  = element_text(size=12), axis.text  = element_text(size=12), strip.text=element_text(size=12))


###Plot EF and FD category descriptions###
layout(matrix(1:4,2,byrow=T))
plot(NA,NA,xlim=c(0,1),ylim=c(-3,3),xlab="relative abundance",ylab="log lambda",main="")
abline(h=0,col="grey",lwd=2)
abline(1,30,lwd=2)
abline(-3,30,lwd=2)
plot(NA,NA,xlim=c(0,1),ylim=c(-3,3),xlab="relative abundance",ylab="log lambda",main="NFD Too Weak")
abline(h=0,col="grey",lwd=2)
abline(-1,-20,lwd=2)
plot(NA,NA,xlim=c(0,1),ylim=c(-3,3),xlab="relative abundance",ylab="log lambda",main="Invading")
abline(h=0,col="grey",lwd=2)
abline(3,-2,lwd=2)
plot(NA,NA,xlim=c(0,1),ylim=c(-3,3),xlab="relative abundance",ylab="log lambda",main="Going Extinct")
abline(h=0,col="grey",lwd=2)
abline(-3,2,lwd=2)

###Compare significant and non-sig communities###
layout(1)
plot(density(log(-persist$slope[which(persist$sig=="Y")]),from=0),lwd=2,main="",xlab="log(NFD)")
lines(density(log(-persist$slope[which(persist$sig=="N")]),from=0),col="red",lwd=2)

plot(density(-persist$slope[which(persist$sig=="Y")],from=0),lwd=2,main="",xlab="NFD",xlim=c(0,15000))
lines(density(-persist$slope[which(persist$sig=="N")],from=0),col="red",lwd=2)

plot(density(persist$intercept[which(persist$sig=="Y")],from=0),lwd=2,main="",xlab="EF")
lines(density(persist$intercept[which(persist$sig=="N")],from=0),col="red",lwd=2)

###Compare significant and non-sig communities - rare species###
persistR=persist[which(persist$rare=="R"),]
plot(density(log(-persistR$slope[which(persistR$sig=="Y")]),from=0),lwd=2,main="",xlab="log(NFD)")
lines(density(log(-persistR$slope[which(persistR$sig=="N")]),from=0),col="red",lwd=2)

plot(density(-persistR$slope[which(persistR$sig=="Y")],from=0),lwd=2,main="",xlab="NFD",xlim=c(0,15000))
lines(density(-persistR$slope[which(persistR$sig=="N")],from=0),col="red",lwd=2)

plot(density(persistR$intercept[which(persistR$sig=="Y")],from=0),lwd=2,main="",xlab="EF")
lines(density(persistR$intercept[which(persistR$sig=="N")],from=0),col="red",lwd=2)

###Compare significant and non-sig communities - common species###
persistC=persist[which(persist$rare=="C"),]
plot(density(log(-persistC$slope[which(persistC$sig=="Y")]),from=0),lwd=2,main="",xlab="log(NFD)",ylim=c(0,0.4))
lines(density(log(-persistC$slope[which(persistC$sig=="N")]),from=0),col="red",lwd=2)

plot(density(-persistC$slope[which(persistC$sig=="Y")],from=0),lwd=2,main="",xlab="NFD",xlim=c(0,15000))
lines(density(-persistC$slope[which(persistC$sig=="N")],from=0),col="red",lwd=2)

plot(density(persistC$intercept[which(persistC$sig=="Y")],from=0),lwd=2,main="",xlab="EF")
lines(density(persistC$intercept[which(persistC$sig=="N")],from=0),col="red",lwd=2)