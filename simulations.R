#This script tests the Empirical NFD methodology against simulated communities with 
#known NFD structure to demonstrate the removal of known biases when determining significance

#################################Create 3 scenarios##########################################
source('./growth.R')
source("./samplingfunction.r")
source("./stabil_function.r") #source stabil function with plotting turned on
layout(matrix(c(1,2,3),1,3))

#Set relative abundances for 10 species, constant for all scenarios
relabunds=c(0.01,0.02,0.03,0.05,0.06,0.09,0.1,0.15,0.2,0.29) 

#Scenario 1 - no NFD structure (10 species)
scen1b=rep(-1,10)
scen1a=-scen1b*relabunds
scen1C=growth("scenario1",scen1a,scen1b)

scen1obsA=sampling(C=scen1C, size=rep(100,10))
scen1resultsA=stabil(scen1obsA,sp_names=1:10,dataname="Scenario 1 (size=100)")

scen1obsB=sampling(C=scen1C, size=1000*relabunds)
scen1resultsB=stabil(scen1obsB,sp_names=1:10,dataname="Scenario 1 (size=1000*f)")

scen1obsC=sampling(C=scen1C, size=10/relabunds)
scen1resultsC=stabil(scen1obsC,sp_names=1:10,dataname="Scenario 1 (size=10/f)")

#Scenario 2 - weak NFD structure (10 species)
scen2b=-c(2.9,2,1.5,1,0.9,0.6,.5,.3,.2,.1)
scen2a=-scen2b*relabunds
scen2C=growth("scenario2",scen2a,scen2b)

scen2obsA=sampling(C=scen2C, size=rep(100,10))
scen2resultsA=stabil(scen2obsA,sp_names=1:10,dataname="Scenario 2 (size=100)")

scen2obsB=sampling(C=scen2C, size=1000*relabunds)
scen2resultsB=stabil(scen2obsB,sp_names=1:10,dataname="Scenario 2 (size=1000*f)")

scen2obsC=sampling(C=scen2C, size=10/relabunds)
scen2resultsC=stabil(scen2obsC,sp_names=1:10,dataname="Scenario 2 (size=10/f)")

#Scenario 3 - strong NFD structure (10 species)
scen3b=-c(2.9,2,1.5,1,0.9,0.6,.5,.3,.2,.1)^2
scen3a=-scen3b*relabunds
scen3C=growth("scenario3",scen3a,scen3b)

scen3obsA=sampling(C=scen3C, size=rep(100,10))
scen3resultsA=stabil(scen3obsA,sp_names=1:10,dataname="Scenario 3 (size=100)")

scen3obsB=sampling(C=scen3C, size=1000*relabunds)
scen3resultsB=stabil(scen3obsB,sp_names=1:10,dataname="Scenario 3 (size=1000*f)")

scen3obsC=sampling(C=scen3C, size=10/relabunds)
scen3resultsC=stabil(scen3obsC,sp_names=1:10,dataname="Scenario 3 (size=10/f)")

#############################Make scenario plots##################################
color=c("springgreen4","royalblue3","tomato","tomato4","yellowgreen","purple4","springgreen",
        "yellow2","thistle4","tan2","violetred3","turquoise1","rosybrown3","palevioletred4",
        "tan4","violet","steelblue4","palevioletred3","thistle2","red4","seagreen4",
        "rosybrown4","paleturquoise4","palegreen4","darksalmon",1:50)

#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="log lambda",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,paste('1)','pattern=',round(lm(log(-scen1b)~log(relabunds))$coefficients[2],3),sep=' '),adj=0,line=0.5); abline(h=0,col="grey")
for(i in 1:length(relabunds)) {
  abline(a=scen1a[i],b=scen1b[i],lwd=2,lty=i,col=color[i]) }

#Scenario 2 - weak NFD structure (10 species)
plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,paste('2)','pattern=',round(lm(log(-scen2b)~log(relabunds))$coefficients[2],3),sep=' '),adj=0,line=0.5); abline(h=0,col="grey")
for(i in 1:length(relabunds)) {
abline(a=scen2a[i],b=scen2b[i],lwd=2,lty=i,col=color[i]) }


#Scenario 3 - strong NFD structure (10 species)
plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,paste('3)','pattern=',round(lm(log(-scen3b)~log(relabunds))$coefficients[2],3),sep=' '),adj=0,line=0.5); abline(h=0,col="grey")
for(i in 1:length(relabunds)) {
  abline(a=scen3a[i],b=scen3b[i],lwd=2,lty=i,col=color[i]) }

##############################Abundance Plots##########################################

#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen1C[,-(1:3)]),xlab="Time",ylab="N",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("1")),adj=0,line=0.5); abline(h=0,col="grey")
for(i in 1:length(relabunds)) {
points(scen1C$Year,scen1C[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 2 - weak NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen2C[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("2")),adj=0,line=0.5); abline(h=0,col="grey")
for(i in 1:length(relabunds)) {
  points(scen2C$Year,scen2C[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 3 - strong NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen3C[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("3")),adj=0,line=0.5); abline(h=0,col="grey")
for(i in 1:length(relabunds)) {
  points(scen3C$Year,scen3C[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

##############################Observed Plots##########################################

#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen1obsC[,-(1:3)]),xlab="Time",ylab="N",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("1")),adj=0,line=0.5); abline(h=0,col="grey")
for(i in 1:length(relabunds)) {
  points(scen1obsC$Year,scen1obsC[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 2 - weak NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen2obsC[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("2")),adj=0,line=0.5); abline(h=0,col="grey")
for(i in 1:length(relabunds)) {
  points(scen2obsC$Year,scen2obsC[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Scenario 3 - strong NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen3obsC[,-(1:3)]),xlab="Time",ylab="",cex.lab=1.5,tcl=0.2, mgp=c(2,0.5,0))
mtext(side=3,expression(paste("3")),adj=0,line=0.5); abline(h=0,col="grey")
for(i in 1:length(relabunds)) {
  points(scen3obsC$Year,scen3obsC[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

