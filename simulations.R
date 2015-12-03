#This script tests the Empirical NFD methodology against simulated communities with 
#known NFD structure to demonstrate the removal of known biases when determining significance

#################################Create 3 scenarios##########################################
source('./growth.R')
source("./samplingfunction.r")
source("./stabil_function.r") #source stabil function with plotting turned on

#Set relative abundances for 10 species, constant for all scenarios
relabunds=c(0.001,0.01,0.02,0.04,0.08,0.09,0.149,0.16,0.18,0.27) 

#Scenario 1 - no NFD structure (10 species)
scen1b=rep(-1,10)
scen1a=-scen1b*relabunds
scen1C=growth("scenario1",scen1a,scen1b)

scen1obsA=sampling(C=scen1C, size=rep(1,10))
scen1resultsA=stabil(scen1obsA,sp_names=1:10,dataname="Scenario 1A (k=1)")

scen1obsB=sampling(C=scen1C, size=10*relabunds)
scen1resultsB=stabil(scen1obsB,sp_names=1:10,dataname="Scenario 1B (k=10*f)")

scen1obsC=sampling(C=scen1C, size=.1/relabunds)
scen1resultsC=stabil(scen1obsC,sp_names=1:10,dataname="Scenario 1C (k=.1/f)")

#Scenario 2 - weak NFD structure (10 species)
scen2b=-exp(-0.5*log(relabunds))
scen2a=-scen2b*relabunds
scen2C=growth("scenario2",scen2a,scen2b)

scen2obsA=sampling(C=scen2C, size=rep(1,10))
scen2resultsA=stabil(scen2obsA,sp_names=1:10,dataname="Scenario 2A (k=1)")

scen2obsB=sampling(C=scen2C, size=10*relabunds)
scen2resultsB=stabil(scen2obsB,sp_names=1:10,dataname="Scenario 2B (k=10*f)")

scen2obsC=sampling(C=scen2C, size=.1/relabunds)
scen2resultsC=stabil(scen2obsC,sp_names=1:10,dataname="Scenario 2C (k=.1/f)")

#Scenario 3 - moderate NFD structure, equal intercepts (10 species)
scen3b=-1/relabunds
scen3a=rep(1,10)
scen3C=growth("scenario3",scen3a,scen3b)

scen3obsA=sampling(C=scen3C, size=rep(1,10))
scen3resultsA=stabil(scen3obsA,sp_names=1:10,dataname="Scenario 3A (k=1)")

scen3obsB=sampling(C=scen3C, size=10*relabunds)
scen3resultsB=stabil(scen3obsB,sp_names=1:10,dataname="Scenario 3B (k=10*f)")

scen3obsC=sampling(C=scen3C, size=.1/relabunds)
scen3resultsC=stabil(scen3obsC,sp_names=1:10,dataname="Scenario 3C (k=.1/f)")

#Scenario 4 - strong NFD structure (10 species)
scen4b=-exp(-1.5*log(relabunds))
scen4a=-scen4b*relabunds
scen4C=growth("scenario4",scen4a,scen4b)

scen4obsA=sampling(C=scen4C, size=rep(1,10))
scen4resultsA=stabil(scen4obsA,sp_names=1:10,dataname="Scenario 4A (k=1)")

scen4obsB=sampling(C=scen4C, size=10*relabunds)
scen4resultsB=stabil(scen4obsB,sp_names=1:10,dataname="Scenario 4B (k=10*f)")

scen4obsC=sampling(C=scen4C, size=.1/relabunds)
scen4resultsC=stabil(scen4obsC,sp_names=1:10,dataname="Scenario 4C (k=.1/f)")

#############################Make scenario plots##################################
layout(matrix(c(1,2,3,4),1,4))
color=c("springgreen4","royalblue3","tomato","tomato4","yellowgreen","purple4","springgreen",
        "yellow2","thistle4","tan2","violetred3","turquoise1","rosybrown3","palevioletred4",
        "tan4","violet","steelblue4","palevioletred3","thistle2","red4","seagreen4",
        "rosybrown4","paleturquoise4","palegreen4","darksalmon",1:50)

#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="log lambda",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,paste('1)','pattern=',round(lm(log(-scen1b)~log(relabunds))$coefficients[2],3),sep=' '),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  abline(a=scen1a[i],b=scen1b[i],lwd=2,lty=i,col=color[i]) }

#Scenario 2 - weak NFD structure (10 species)
plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,paste('2)','pattern=',round(lm(log(-scen2b)~log(relabunds))$coefficients[2],3),sep=' '),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
abline(a=scen2a[i],b=scen2b[i],lwd=2,lty=i,col=color[i]) }


#Scenario 3 - moderate NFD structure, equal intercepts (10 species)
plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,paste('3)','pattern=',round(lm(log(-scen3b)~log(relabunds))$coefficients[2],3),sep=' '),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  abline(a=scen3a[i],b=scen3b[i],lwd=2,lty=i,col=color[i]) }

#Scenario 4 - strong NFD structure (10 species)
plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,paste('4)','pattern=',round(lm(log(-scen4b)~log(relabunds))$coefficients[2],3),sep=' '),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  abline(a=scen4a[i],b=scen4b[i],lwd=2,lty=i,col=color[i]) }

##############################Abundance Plots##########################################

#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen1C[,-(1:3)]),xlab="Time",ylab="N",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("1")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
points(scen1C$Year,scen1C[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 2 - weak NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen2C[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("2")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen2C$Year,scen2C[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 3 - moderate NFD structure, equal intercepts (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen3C[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("3")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen3C$Year,scen3C[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 4 - strong NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen4C[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("4")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen4C$Year,scen4C[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

##############################Observed Plots##########################################
layout(rbind(1:4,5:8,9:12))

#Sampling A k = 1
#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen1obsA[,-(1:3)]), xlab="Time",ylab="N",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0),main="Sampling A (k=1)",cex.main=1.7)
mtext(side=3,expression(paste("1")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen1obsA$Year,scen1obsA[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 2 - weak NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen2obsA[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("2")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen2obsA$Year,scen2obsA[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 3 - moderate NFD structure, equal intercepts (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen3obsA[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("3")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen3obsA$Year,scen3obsA[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 4 - strong NFD structure (10 species)
plot(NA,NA,xlim=c(0,21),ylim=range(scen4obsA[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("4")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen4obsA$Year,scen4obsA[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Sampling B k = 10*f
#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen1obsB[,-(1:3)]), xlab="Time",ylab="N",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0),main="Sampling B (k=10*f)",cex.main=1.7)
mtext(side=3,expression(paste("1")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen1obsB$Year,scen1obsB[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 2 - weak NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen2obsB[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("2")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen2obsB$Year,scen2obsB[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 3 - moderate NFD structure, equal intercepts (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen3obsB[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("3")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen3obsB$Year,scen3obsB[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 4 - strong NFD structure (10 species)
plot(NA,NA,xlim=c(0,21),ylim=range(scen4obsB[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("4")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen4obsB$Year,scen4obsB[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Sampling C k = .1/f
#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen1obsC[,-(1:3)]), xlab="Time",ylab="N",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0),main="Sampling C (k=.1/f)",cex.main=1.7)
mtext(side=3,expression(paste("1")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen1obsC$Year,scen1obsC[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 2 - weak NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen2obsC[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("2")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen2obsC$Year,scen2obsC[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 3 - moderate NFD structure, equal intercepts (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen3obsC[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("3")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen3obsC$Year,scen3obsC[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 4 - strong NFD structure (10 species)
plot(NA,NA,xlim=c(0,21),ylim=range(scen4obsC[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("4")),adj=0,line=0.5,cex=1.3); abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen4obsC$Year,scen4obsC[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }