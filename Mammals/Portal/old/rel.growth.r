#log lambda function
lambda=function(file) {
if(file[i+1,x]==0 | file[i,x]==0){
  r=NA
}else{
  r=log(file[i+1,x]/file[i,x])
}
return(r)
}

#pre-95
#controls89
lastyear1=1994

#files
controls89=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\data\\Portal\\Controls_Crosstab89.txt", header=T)
controlE89=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\data\\Portal\\Energy_Controls89.txt", header=T)

#calc relative E
relE.con89=controlE89[,4:10]/controlE89[,3]
relE.con89=relE.con89[which(controls89[,1]!=lastyear1),]
#calc lambda
con.r89=matrix(data=NA,nrow=length(controls89[,1])-1,ncol=length(controls89[1,]))
for(i in 1:(length(controls89[,1])-1)) {
  for(x in 1:length(controls89[1,])) {
    con.r89[i,x]=lambda(controls89)
  }
}
con.r89=con.r89[which(controls89[,1]!=lastyear1),] 

plot(relE.con89[,2],con.r89[,5],xlim=c(0,1),ylim=c(-3,3),main="controls89",xlab="relative E",ylab="log lambda")
points(relE.con89[,3],con.r89[,6],pch=2,col=2)
points(relE.con89[,4],con.r89[,7],pch=3,col=3)
points(relE.con89[,5],con.r89[,8],pch=4,col=4)
points(relE.con89[,6],con.r89[,9],pch=5,col=5)
points(relE.con89[,7],con.r89[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r89[,5]~relE.con89[,2]),col=1)
abline(lm(con.r89[,6]~relE.con89[,3]),col=2)
abline(lm(con.r89[,7]~relE.con89[,4]),col=3)
abline(lm(con.r89[,8]~relE.con89[,5]),col=4)
abline(lm(con.r89[,9]~relE.con89[,6]),col=5)
abline(lm(con.r89[,10]~relE.con89[,7]),col=6)

legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con89[,2],con.r89[,5])
#DO
cor.test(relE.con89[,3],con.r89[,6])
#DS
cor.test(relE.con89[,4],con.r89[,7])
#PB
cor.test(relE.con89[,5],con.r89[,8])
#PF
cor.test(relE.con89[,6],con.r89[,9])
#PP
cor.test(relE.con89[,7],con.r89[,10])
#AO
cor.test(relE.con89[,1],con.r89[,4])

#exclosures89

#files
exclosures89=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\data\\Portal\\Exclosures_Crosstab89.txt", header=T)
exclosureE89=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\data\\Portal\\Energy_exclosures89.txt", header=T)

#calc relative E
relE.ex89=exclosureE89[,4:7]/exclosureE89[,3]
relE.ex89=relE.ex89[which(exclosures89[,1]!=lastyear1),]

#calc lambda
ex.r89=matrix(data=NA,nrow=length(exclosures89[,1])-1,ncol=length(exclosures89[1,]))
for(i in 1:(length(exclosures89[,1])-1)) {
  for(x in 1:length(exclosures89[1,])) {
    ex.r89[i,x]=lambda(exclosures89)
  }
}
ex.r89=ex.r89[which(exclosures89[,1]!=lastyear1),]  

plot(relE.ex89[,2],ex.r89[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main="exclosures89",xlab="relative E",ylab="log lambda")
points(relE.ex89[,3],ex.r89[,6],pch=5,col=5)
points(relE.ex89[,4],ex.r89[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r89[,5]~relE.ex89[,2]),col=4)
abline(lm(ex.r89[,6]~relE.ex89[,3]),col=5)
abline(lm(ex.r89[,7]~relE.ex89[,4]),col=6)

legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

points(mean(relE.ex89[,3]),mean(ex.r89[,4],na.rm=T),col=5,pch=5,cex=2,lwd=3)
points(mean(relE.ex89[,4]),mean(ex.r89[,7],na.rm=T),col=6,pch=6,cex=2,lwd=3)

#PB
cor.test(relE.ex89[,2],ex.r89[,5])
#PF
cor.test(relE.ex89[,3],ex.r89[,6])
#PP
cor.test(relE.ex89[,4],ex.r89[,7])
#AO
cor.test(relE.ex89[,1],ex.r89[,4])



#post-95
#controls95
lastyear2=2007

#files
controls95=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\data\\Portal\\Controls_Crosstab95.txt", header=T)
controlE95=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\data\\Portal\\Energy_Controls95.txt", header=T)

#calc relative E
relE.con95=controlE95[,4:10]/controlE95[,3]
relE.con95=relE.con95[which(controls95[,1]!=lastyear2),]

#calc lambda
con.r95=matrix(data=NA,nrow=length(controls95[,1])-1,ncol=length(controls95[1,]))
for(i in 1:(length(controls95[,1])-1)) {
  for(x in 1:length(controls95[1,])) {
    con.r95[i,x]=lambda(controls95)
  }
}
con.r95=con.r95[which(controls95[,1]!=lastyear2),] 

plot(relE.con95[,2],con.r95[,5],xlim=c(0,1),ylim=c(-3,3),main="controls95",xlab="relative E",ylab="log lambda")
points(relE.con95[,3],con.r95[,6],pch=2,col=2)
points(relE.con95[,4],con.r95[,7],pch=3,col=3)
points(relE.con95[,5],con.r95[,8],pch=4,col=4)
points(relE.con95[,6],con.r95[,9],pch=5,col=5)
points(relE.con95[,7],con.r95[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r95[,5]~relE.con95[,2]),col=1)
abline(lm(con.r95[,6]~relE.con95[,3]),col=2)
abline(lm(con.r95[,7]~relE.con95[,4]),col=3)
abline(lm(con.r95[,8]~relE.con95[,5]),col=4)
abline(lm(con.r95[,9]~relE.con95[,6]),col=5)
abline(lm(con.r95[,10]~relE.con95[,7]),col=6)

legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con95[,2],con.r95[,5])
#DO
cor.test(relE.con95[,3],con.r95[,6])
#DS
cor.test(relE.con95[,4],con.r95[,7])
#PB
cor.test(relE.con95[,5],con.r95[,8])
#PF
cor.test(relE.con95[,6],con.r95[,9])
#PP
cor.test(relE.con95[,7],con.r95[,10])
#AO
cor.test(relE.con95[,1],con.r95[,4])

#exclosures95

#files
exclosures95=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\data\\Portal\\Exclosures_Crosstab95.txt", header=T)
exclosureE95=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\data\\Portal\\Energy_exclosures95.txt", header=T)

#calc relative E
relE.ex95=exclosureE95[,4:7]/exclosureE95[,3]
relE.ex95=relE.ex95[which(exclosures95[,1]!=lastyear2),]
#calc lambda
ex.r95=matrix(data=NA,nrow=length(exclosures95[,1])-1,ncol=length(exclosures95[1,]))
for(i in 1:(length(exclosures95[,1])-1)) {
  for(x in 1:length(exclosures95[1,])) {
    ex.r95[i,x]=lambda(exclosures95)
  }
}
ex.r95=ex.r95[which(exclosures95[,1]!=lastyear2),]  

plot(relE.ex95[,2],ex.r95[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main="exclosures95",xlab="relative E",ylab="log lambda")
points(relE.ex95[,3],ex.r95[,6],pch=5,col=5)
points(relE.ex95[,4],ex.r95[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r95[,5]~relE.ex95[,2]),col=4)
abline(lm(ex.r95[,6]~relE.ex95[,3]),col=5)
abline(lm(ex.r95[,7]~relE.ex95[,4]),col=6)

legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

points(mean(relE.ex95[,2]),mean(ex.r95[,5],na.rm=T),col=4,pch=4,cex=2,lwd=3)
points(mean(relE.ex95[,3]),mean(ex.r95[,4],na.rm=T),col=5,pch=5,cex=2,lwd=3)
points(mean(relE.ex95[,4]),mean(ex.r95[,7],na.rm=T),col=6,pch=6,cex=2,lwd=3)

#PB
cor.test(relE.ex95[,2],ex.r95[,5])
#PF
cor.test(relE.ex95[,3],ex.r95[,6])
#PP
cor.test(relE.ex95[,4],ex.r95[,7])
#AO
cor.test(relE.ex95[,1],ex.r95[,4])