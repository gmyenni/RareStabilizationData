#log lambda function
lambda=function(file) {
if(file[i+1,x]==0 | file[i,x]==0){
  r=NA
}else{
  r=log(file[i+1,x]/file[i,x])
}
return(r)
}

#Controls
  
  for(t in 1989:2006){

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

#by year
relE.con=relE.con[which(controlE[,1]==t),]
con.r=con.r[which(controls[,1]==t+1),]     


plot(relE.con[,2],con.r[,5],xlim=c(0,1),ylim=c(-3,3),main="Controls",xlab="relative E",ylab="log lambda")
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

}