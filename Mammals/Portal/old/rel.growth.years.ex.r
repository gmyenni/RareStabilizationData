#log lambda function
lambda=function(file) {
if(file[i+1,x]==0 | file[i,x]==0){
  r=NA
}else{
  r=log(file[i+1,x]/file[i,x])
}
return(r)
}

layout(matrix(1:18,3,byrow=T))

#Exclosures 1989
t=1989

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main="1989",xlab=NA,ylab="log lambda",xaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])



#Exclosures 1990
t=1990

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main="1990",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 1991
t=1991

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main="1991",,xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 1992
t=1992

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])

#Exclosures 1993
t=1993

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])

#Exclosures 1994
t=1994

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 1995
t=1995

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab=NA,ylab="log lambda",xaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 1996
t=1996

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 1997
t=1997

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 1998
t=1998

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 1999
t=1999

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 2000
t=2000

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 2001
t=2001

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab="relative E",ylab="log lambda")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 2002
t=2002

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab="relative E",ylab=NA,yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 2003
t=2003

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab="relative E",ylab=NA,yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 2004
t=2004

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab="relative E",ylab=NA,yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 2005
t=2005

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab="relative E",ylab=NA,yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

#legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])


#Exclosures 2006
t=2006

#files
exclosures=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Exclosures_Crosstab.txt", header=T)
exclosureE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_exclosures.txt", header=T)

#calc relative E
relE.ex=exclosureE[,4:7]/exclosureE[,3]
#calc lambda
ex.r=matrix(data=NA,nrow=length(exclosures[,1])-1,ncol=length(exclosures[1,]))
for(i in 1:(length(exclosures[,1])-1)) {
  for(x in 1:length(exclosures[1,])) {
    ex.r[i,x]=lambda(exclosures)
  }
}

#by year
relE.ex=relE.ex[which(exclosureE[,1]==t),]
ex.r=ex.r[which(exclosures[,1]==t),]


plot(relE.ex[,2],ex.r[,5],pch=4,col=4, xlim=c(0,1),ylim=c(-3,3),main=t,xlab="relative E",ylab=NA,yaxt="n")
points(relE.ex[,3],ex.r[,6],pch=5,col=5)
points(relE.ex[,4],ex.r[,7],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(ex.r[,5]~relE.ex[,2]),col=4)
abline(lm(ex.r[,6]~relE.ex[,3]),col=5)
abline(lm(ex.r[,7]~relE.ex[,4]),col=6)

legend(x=0.9,y=3,legend=c('PB','PF','PP'),col=4:6,pch=4:6)

#PB
cor.test(relE.ex[,2],ex.r[,5])
#PF
cor.test(relE.ex[,3],ex.r[,6])
#PP
cor.test(relE.ex[,4],ex.r[,7])
#AO
cor.test(relE.ex[,1],ex.r[,4])