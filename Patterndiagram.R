layout(matrix(c(1,2),1,2))

plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="log Growth Rate",
     cex.lab=1.1,main="Frequency-dependence")
abline(h=0,col="grey")
abline(a=0.3,b=-3,lwd=2,lty=1)
abline(a=0.6,b=-2,lwd=2,lty=2)
abline(a=0.6,b=-1,lwd=2,lty=3)

curve(sin, xlim=c(0,10), main="Compensatory Dynamics",ylab="change in N",lwd=2,xlab="Time",cex.lab=1.1)
curve(cos(x+pi/4+pi/8),add=TRUE,lty=2,lwd=2)
diff=function(x) sin(x)+cos(x+pi/4+pi/8)
curve(diff, xname = "Time",add=TRUE,lty=4,lwd=4)