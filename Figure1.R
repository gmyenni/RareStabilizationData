#plot scenarios
layout(matrix(c(1,2,3,4),1,4))

plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="log growth rate",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("1")),adj=0,line=0.5)
abline(h=0,col="grey")
abline(a=0.1,b=-1,lwd=2,lty=1)
abline(a=0.6,b=-2,lwd=2,lty=2)
abline(a=1.8,b=-3,lwd=2,lty=3)

plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("2")),adj=0,line=0.5)
abline(h=0,col="grey")
abline(a=0.3,b=-3,lwd=2,lty=1)
abline(a=1.2,b=-3,lwd=2,lty=2)
abline(a=1.8,b=-3,lwd=2,lty=3)

plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("3")),adj=0,line=0.5)
abline(h=0,col="grey")
abline(a=0.3,b=-3,lwd=2,lty=1)
abline(a=0.6,b=-1.5,lwd=2,lty=2)
abline(a=1.8,b=-3,lwd=2,lty=3)

plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("4")),adj=0,line=0.5)
abline(h=0,col="grey")
abline(a=0.3,b=-3,lwd=2,lty=1)
abline(a=0.6,b=-2,lwd=2,lty=2)
abline(a=0.6,b=-1,lwd=2,lty=3)





