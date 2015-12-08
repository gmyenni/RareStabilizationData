#This script tests the Empirical NFD methodology against simulated communities with 
#known NFD structure AND IMMIGRATION to demonstrate the removal of known biases when 
#determining significance

#################################Create 2 scenarios##########################################
source('./immigration.R')
source("./samplingfunction.r")
source("./stabil_function.r") #source stabil function with plotting turned on

#Set relative abundances for 10 species, constant for all scenarios
relabunds=c(0.001,0.01,0.02,0.04,0.08,0.09,0.149,0.16,0.18,0.27) 

#Scenario 1 - no NFD structure (10 species)
scen1b=rep(-1,10)
scen1a=-scen1b*relabunds

scen1C=immigration("scenario1",scen1a,scen1b)
scen1results=stabil(scen1C,sp_names=1:10,dataname="Focal Community")

scen1obsA=sampling(C=scen1C, size=rep(1,10))
scen1resultsA=stabil(scen1obsA,sp_names=1:10,dataname="Sampling A (k=1)")

scen1obsB=sampling(C=scen1C, size=10*relabunds)
scen1resultsB=stabil(scen1obsB,sp_names=1:10,dataname="Sampling B (k=10*f)")

scen1obsC=sampling(C=scen1C, size=.1/relabunds)
scen1resultsC=stabil(scen1obsC,sp_names=1:10,dataname="Sampling C (k=.1/f)")

#Scenario 2 - randomly assigned parameters (10 species)
scen1b=seq(-1.9,-1,by=0.1); scen1a=-scen1b*relabunds
scen1a=sample(scen1a); scen1b=sample(scen1b)

scen1C=immigration("scenario1",scen1a,scen1b)
scen1results=stabil(scen1C,sp_names=1:10,dataname="Focal Community")

scen1obsA=sampling(C=scen1C, size=rep(1,10))
scen1resultsA=stabil(scen1obsA,sp_names=1:10,dataname="Sampling A (k=1)")

scen1obsB=sampling(C=scen1C, size=10*relabunds)
scen1resultsB=stabil(scen1obsB,sp_names=1:10,dataname="Sampling B (k=10*f)")

scen1obsC=sampling(C=scen1C, size=.1/relabunds)
scen1resultsC=stabil(scen1obsC,sp_names=1:10,dataname="Sampling C (k=.1/f)")


#############################Plot Simulated Data##################################
color=c("springgreen4","royalblue3","tomato","tomato4","yellowgreen","purple4","springgreen",
        "yellow2","thistle4","tan2","violetred3","turquoise1","rosybrown3","palevioletred4",
        "tan4","violet","steelblue4","palevioletred3","thistle2","red4","seagreen4",
        "rosybrown4","paleturquoise4","palegreen4","darksalmon",1:50)

#############################Make scenario plots##################################
layout(matrix(c(1,2),1,2))
#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,1), ylim=c(-1,1),xlab="Frequency",ylab="log lambda",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  abline(a=scen1a[i],b=scen1b[i],lwd=2,lty=i,col=color[i]) }

##############################Abundance Plots##########################################

#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen1C[,-(1:3)]),xlab="Time",ylab="N",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0))
abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen1C$Year,scen1C[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

##############################Observed Plots##########################################
layout(matrix(1:3,1,3))

#Sampling A k = 1
#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen1obsA[,-(1:3)]), xlab="Time",ylab="N",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0),main="Sampling A (k=1)",cex.main=1.7)
abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen1obsA$Year,scen1obsA[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }

#Sampling B k = 10*f
#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen1obsB[,-(1:3)]), xlab="Time",ylab="N",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0),main="Sampling B (k=10*f)",cex.main=1.7)
abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen1obsB$Year,scen1obsB[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }


#Sampling C k = .1/f
#Scenario 1 - no NFD structure (10 species)
plot(NA,NA,xlim=c(0,21), ylim=range(scen1obsC[,-(1:3)]), xlab="Time",ylab="N",cex.lab=1.7,cex.axis=1.5,tcl=0.2, mgp=c(2,0.5,0),main="Sampling C (k=.1/f)",cex.main=1.7)
abline(h=0,col="grey",lwd=2)
for(i in 1:length(relabunds)) {
  points(scen1obsC$Year,scen1obsC[,i+3],lwd=2,lty=i,type="b",pch=i,col=color[i]) }
