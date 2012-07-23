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

#Controls  1989
t=1989

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1989",ylab="log lambda",xlab=NA,xaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  1990
t=1990

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1990",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])

#Controls  1991
t=1991

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1991",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])

#Controls  1992
t=1992

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1992",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])

#Controls  1993
t=1993

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1993",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  1994
t=1994

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1994",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  1995
t=1995

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1995",ylab="log lambda",xlab=NA,xaxt="n",)
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])

#Controls  1996
t=1996

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1996",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  1997
t=1997

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1997",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  1998
t=1998

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1998",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  1999
t=1999

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="1999",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  2000
t=2000

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="2000",xlab=NA,ylab=NA,xaxt="n",yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  2001
t=2001

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="2001",xlab="relative E",ylab="log lambda")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  2002
t=2002

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="2002",xlab="relative E",ylab=NA,yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  2003
t=2003

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="2003",xlab="relative E",ylab=NA,yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])


#Controls  2004
t=2004

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="2004",xlab="relative E",ylab=NA,yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])



#Controls  2005
t=2005

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}

#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="2005",xlab="relative E",ylab=NA,yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

#legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])



#Controls  2006
t=2006

#files
controls=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Controls_Crosstab.txt", header=T)
controlE=read.table("C:\\Documents and Settings\\gmyenni.BNR-138-DEL\\My Documents\\Portal\\projects\\niche4neutrality\\Energy_Controls.txt", header=T)

#calc relative E
relE.con=controlE[,4:10]/controlE[,3]
#calc lambda
con.r=matrix(data=NA,nrow=length(controls[,1])-1,ncol=length(controls[1,]))
for(i in 1:(length(controls[,1])-1)) {
  for(x in 1:length(controls[1,])) {
    con.r[i,x]=lambda(controls)
  }
}
                                            
                                               
#year only
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t),]

plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="2006",xlab="relative E",ylab=NA,yaxt="n")
points(relE.con[,3],con.r[,6],pch=2,col=2)
points(relE.con[,4],con.r[,7],pch=3,col=3)
points(relE.con[,5],con.r[,8],pch=4,col=4)
points(relE.con[,6],con.r[,9],pch=5,col=5)
points(relE.con[,7],con.r[,10],pch=6,col=6)
abline(h=0,col="grey")

abline(lm(con.r[,5]~relE.con[,2]),col=1)
abline(lm(con.r[,6]~relE.con[,3]),col=2)
abline(lm(con.r[,7]~relE.con[,4]),col=3)
abline(lm(con.r[,8]~relE.con[,5]),col=4)
abline(lm(con.r[,9]~relE.con[,6]),col=5)
abline(lm(con.r[,10]~relE.con[,7]),col=6)

legend(x=0.9,y=3,legend=c('DM','DO','DS','PB','PF','PP'),col=1:6,pch=1:6)

#DM
cor.test(relE.con[,2],con.r[,5])
#DO
cor.test(relE.con[,3],con.r[,6])
#DS
cor.test(relE.con[,4],con.r[,7])
#PB
cor.test(relE.con[,5],con.r[,8])
#PF
cor.test(relE.con[,6],con.r[,9])
#PP
cor.test(relE.con[,7],con.r[,10])
#AO
cor.test(relE.con[,1],con.r[,4])