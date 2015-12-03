library('ggplot2')

results=read.csv("./AllResults.csv",header=T)
#results=read.csv("./nonlinearresults.csv",header=T)
results$Pcat[which(is.na(results$intercept))]=NA
pattern=read.csv("./AllPattern_in.csv",header=T)
#pattern=read.csv("./nonlinearpattern_in.csv",header=T)

#Get y-intercepts
results$Yint=-results$slope*results$intercept

#Get Portal results for example
relA=read.csv("./PortalrelA.csv",header=T)
rates=read.csv("./Portalrates.csv",header=T)
null_pattern=read.csv("./Portalnull.csv",header=T,dec=".",colClasses="numeric")


#Perform false discovery rate control to get corrected p-values
rand_results=read.csv("background.csv",header=T)
library("fdrtool")
q.val=fdrtool(pattern$p.val, statistic="pvalue")$qval
pattern$q.val=q.val

#Label significant communities
pattern$sigcat[which(pattern$q.val<0.1)]=" Significant"
pattern$sigcat[which(pattern$q.val>0.1)]="Nonsignificant"
sig.list=pattern$Site[which(pattern$sigcat==" Significant")]
nonsig.list=pattern$Site[which(pattern$sigcat=="Nonsignificant")]
results$sigcat=" Significant"
for(i in 1:length(results$site)) { if(results$site[i] %in% nonsig.list) results$sigcat[i]="Nonsignificant" }

#Calculate effect size of pattern
pattern$Srat=pattern$Persistent.S/pattern$Observed.S
pattern$Pdiff=pattern$Pattern-pattern$Null.Pattern

#Save percentages of significant communities in each group
percents=as.data.frame(prop.table(table(pattern$Group,pattern$sigcat),1))
library(doBy)
maxS=summaryBy(Persistent.S ~ Group, data = pattern, FUN = max)
maxPrat=summaryBy(X..increase.in.Pattern ~ Group, data = pattern, FUN = max)
percents=cbind(percents,maxS[,2],maxPrat[,2])
colnames(percents)=c("Group","sigcat","per","maxS","maxPrat")
# birds <- subset(results, Group == "Birds")
# fish <- subset(results, Group == "Fish")
# herps <- subset(results, Group == "Herps")
# inverts <- subset(results, Group == "Invertebrates")
# mammals <- subset(results, Group == "Mammals")
# plants <- subset(results, Group == "Plants")

write.table(pattern,"./AllPattern_out.csv",sep=",", col.names = T, row.names=F)
###############################Figure 2: Portal Rodents example#####################################
###############################Figure 2a: freqency vs growth rate example##########################
color=c("springgreen4","royalblue3","tomato","tomato4","yellowgreen","purple4","springgreen","yellow2","orange")
symbol=c(15,16,17,18,25,8,11,9,10)
plot(NA,NA,xlim=c(0,1),ylim=range(rates,na.rm=T),xaxs="i",yaxs="i",xlab="Relative Abundance",ylab=expression( paste('Growth Rate (log  ', lambda,')')),
     main="Portal Rodents",cex.lab=1.5,cex.axis=1.5,mgp = c(2, 0.5, 0))
abline(h=0,col="grey",lwd=3)

for(i in 1:9){
  points(relA[,i],rates[,i],pch=symbol[i],col=color[i],cex=0.75)
  abline(lm(rates[,i]~relA[,i]),col=color[i],lwd=2.5) }

legend(0.87,3.45,legend=c("DM","DO","DS","PB","PE","PF","PM","PP","RM"),xjust=0,col=color,pch=symbol,lwd=2,cex=0.9,bty='n',title="Species")

###############################Figure 2b: Randomization example##################################
ggplot(subset(results,site=="Portal")) + 
  geom_point(data=subset(rand_results,Site=="Portal"),aes(Intercept,-Slope),colour="darkgrey",size=4) + 
  stat_smooth(data=subset(rand_results,Site=="Portal"),aes(Intercept,-Slope),level=0.95,method="lm", fill="black", colour="black", size=2,linetype=2) + 
  stat_smooth(data=subset(results,site=="Portal"),aes(intercept,-slope), fill="black", colour="black",level=0.95,method="lm", size=2) +
  geom_point(data=subset(results,site=="Portal"),aes(intercept,-slope,colour=species,shape=species),size=5) +
  scale_y_log10('Strength of NFD') + 
  scale_colour_manual(name="Species",values=color) + scale_shape_manual(name="Species",values=symbol) +
  scale_x_log10('Equilibrium frequency',limits=c(0.03,0.4),breaks=c(0.05,0.1,0.2,0.3,0.4),labels=c(0.05,0.1,0.2,0.3,0.4)) +
  #geom_text(data=subset(pattern,Site=="Portal"),
            #aes(x=rep(0.1,5), y=c(100,75,60,47,35),
                #label=c("Portal Rodents","Covariance = -1.17","Mean Randomized Cov = -0.46" , "Observed Cov/Randomized Cov = 2.54","p-val = 0.0032")), colour="black", size=c(7,6,6,6,6)) +

  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.text = element_text(size = 16),legend.key = element_rect(colour = NA),
        legend.justification=c(1,1), legend.position=c(1,1),legend.title = element_text(size=16),
        strip.text = element_text(size=18),
        axis.title.y  = element_text(angle=90,size=22),
        axis.title.x  = element_text(vjust=-.25,hjust=.55,size=22),
        axis.text.y  = element_text(size=18),
        axis.text.x  = element_text(size=18))

###############################Figure 2c: Randomization histogram##################################
Portal=subset(pattern,Site=="Portal")
plot(density(null_pattern[,1]),main="Distribution of Randomized Covariances",xlab="Covariance",lwd=2,cex.lab=1.5,mgp = c(2, 0.5, 0))
abline(v=Portal$Pattern,col='red',lwd=3)
mtext(side=3,paste('p-val=',round(Portal$p.val,4),sep=" "),adj=0,line=0.5,cex=1,col="red")

###############################Figure 3a: All Results, pattern ratios##########################
ggplot(data=pattern,aes(x=X..increase.in.Pattern)) + 
  geom_histogram(aes(fill = sigcat), alpha = 0.8,binwidth = 0.5,position="identity") +
  theme_bw() + scale_fill_manual(values=c("#CC79A7","darkgrey")) +
  scale_x_continuous(name = "Observed Covariance/Randomized Covariance") + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.text = element_text(size = 16),legend.key = element_rect(colour = NA),
        legend.justification=c(1,1), legend.position=c(1,1),legend.title = element_blank(),
        strip.text = element_text(size=16),
        axis.title.y  = element_text(angle=90,size=18),
        axis.title.x  = element_text(vjust=-.25,hjust=.55,size=18),
        axis.text.y  = element_text(size=16),axis.text.x  = element_text(size=16))

###############################Figure 3b: By Group, pattern ratios##########################
ggplot(data=pattern,aes(x=X..increase.in.Pattern)) + 
  scale_fill_brewer(palette="Set1") +
  geom_histogram(data=subset(pattern,sigcat == "Nonsignificant"),fill = "grey", alpha = 1,binwidth = 0.25) +
  geom_histogram(data=subset(pattern,sigcat == " Significant"),aes(fill = Group), alpha = 0.7,binwidth = 0.25) +
  theme_bw() + facet_wrap(~Group,scales = "free_x") +
  geom_text(data=subset(percents,sigcat==" Significant"), aes(x=c(2.25,2,2,5,11,4),y=6,label=paste("Proportion","Significant", ":",round(per,2),sep="")), parse=TRUE, hjust=1, colour="black", size=5) +
  scale_x_continuous(name = "Observed Covariance/Randomized Covariance") + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.position="none",
        strip.text = element_text(size=16),
        axis.title.y  = element_text(angle=90,size=18),
        axis.title.x  = element_text(vjust=-.25,hjust=.55,size=18),
        axis.text.y  = element_text(size=16),axis.text.x  = element_text(size=16))

##################################Supplement Figures##########################################
###############################All Results, with lm's###################################
ggplot(results) + 
  stat_smooth(data=results,aes(intercept,-slope,factor(site),color=sigcat),method="lm", se = FALSE, size=2) + scale_colour_grey(start=0,end=0.6) + 
  scale_y_log10('Strength of NFD',breaks=c(0.00001,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0.00001,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.text = element_text(size = 16),legend.key = element_rect(colour = NA),
        legend.justification=c(1,1), legend.position=c(1,1),legend.title = element_blank(),
        strip.text = element_text(size=16),
        axis.title.y  = element_text(angle=90,size=18),
        axis.title.x  = element_text(vjust=-.25,hjust=.55,size=18),
        axis.text.y  = element_text(size=16),
        axis.text.x  = element_text(size=16))
####################Old Figure 3: Community Results, by group, with lm's###################################
ggplot(results) + facet_wrap(~Group) + scale_colour_discrete(h = c(15, 355), c = 200, l = 20) + 
  #geom_point(data=results,aes(intercept,-slope,colour=site)) + 
  stat_smooth(data=results,aes(intercept,-slope,colour=site, linetype=sigcat),method="lm", se = FALSE, size=1) + 
  scale_y_log10('Strength of NFD',breaks=c(0.00001,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0.00001,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  geom_text(data=subset(percents,sigcat==" Significant"), aes(x=0.5, y=1000000, label=round(per,2)), parse=TRUE, colour="black", size=6) + theme_bw() +
  geom_text(data=NULL,aes(x=0.002, y=1000000,label="Fraction of Significant Communities ="),size=3.6) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.position = "none", strip.text = element_text(size=18),
        axis.title.y  = element_text(angle=90,size=18),
        axis.title.x  = element_text(vjust=-.25,hjust=.55,size=16),
        axis.text.y  = element_text(size=16),
        axis.text.x  = element_text(size=16), strip.text = element_text(size=18))
################################Old Figure 2 (two separate figures)###########################
ggplot(results) +  
  stat_smooth(data=rand_results,aes(Intercept,-Slope),method="lm", fill="black", colour="black", size=1.5,level=0.99,alpha=0.8,linetype = 2) +
  geom_point(data=results,aes(intercept,-slope,colour=Group),size=2.5) + scale_colour_brewer(palette="Set1")  +
  stat_smooth(data=results,aes(intercept,-slope),method="lm", fill="black", colour="black", size=1.5,level=0.99,alpha=0.8) +
  scale_y_log10('Strength of NFD',breaks=c(0.00001,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000'),limits=c(0.1,100000)) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0.00001,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.text = element_text(size = 16),legend.key = element_rect(colour = NA),
        legend.justification=c(1,1), legend.position=c(1,1),legend.title = element_text(size=16),
        axis.title.y  = element_text(angle=90,size=16),
        axis.title.x  = element_text(vjust=-.25,hjust=.55,size=16),
        axis.text.y  = element_text(size=16),
        axis.text.x  = element_text(size=16))

ggplot(rand_results) + geom_point(data=rand_results,aes(Intercept,-Slope),colour="darkgrey") + 
  stat_smooth(data=rand_results,aes(Intercept,-Slope),method="lm", fill="black", colour="black", size=1.5,level=0.99,alpha=0.8,linetype = 2) +
  scale_y_log10('Strength of NFD',breaks=c(0.00001,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000'),limits=c(0.1,100000)) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0.00001,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.text = element_text(size = 16),legend.key = element_rect(colour = NA),
        legend.position="right", legend.title = element_text(size=16),
        axis.title.y  = element_text(angle=90,size=16),
        axis.title.x  = element_text(vjust=-.25,hjust=.55,size=16),
        axis.text.y  = element_text(size=16),
        axis.text.x  = element_text(size=16))

##############################p-value for Figure 2#############################
fullm=lm(log(-slope)~log(intercept),data=results)
nullm=lm(log(-Slope)~log(Intercept),data=rand_results)
anova(nullm,fullm)


###############################All Results, by group, with lm's###################################
ggplot(results) + geom_point(data=rand_results,aes(Intercept,-Slope),colour="darkgrey") + 
  stat_smooth(data=rand_results,aes(Intercept,-Slope),method="lm", fill="black", colour="black", size=2) +
  geom_point(data=results,aes(intercept,-slope,colour=Group)) + scale_colour_brewer(palette="Set1")  +
  stat_smooth(data=results,aes(intercept,-slope,colour=Group),method="lm", fill="black", size=2) +
  scale_y_log10('Strength of NFD',breaks=c(0.00001,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0.00001,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  facet_wrap(~Group) + geom_text(data=percents, aes(x=0.6, y=100000, label=per), parse=TRUE, colour="black", size=6) + theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.position = "none", strip.text = element_text(size=16),
        axis.title.y  = element_text(angle=90,size=16),
        axis.title.x  = element_text(vjust=-.25,hjust=.55,size=16),
        axis.text.y  = element_text(size=16),
        axis.text.x  = element_text(size=16))

#############################y-intercept figures#############################################
ggplot(subset(results,Pcat=="P")) + facet_wrap(~Group) +
  geom_point(data=subset(results,Pcat=="P"),aes(intercept,Yint,colour=site)) + 
  #stat_smooth(data=subset(results,Pcat=="P"),aes(intercept,Yint,colour=site, linetype=sigcat),method="lm", se = FALSE, size=2) + scale_colour_discrete(h = c(15, 360), c = 200, l = 35) +
  scale_y_continuous('Y intercept', limits=c(0,15)) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0.00001,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  #geom_text(data=percents, aes(x=0.5, y=15, label=per), parse=TRUE, colour="black", size=6) + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.position = "none", strip.text = element_text(size=16),
        axis.title.y = element_text(angle=90,size=16),
        axis.title.x = element_text(vjust=-.25,hjust=.55,size=16),
        axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=16))

###########################################################################################
#####################################Other figures#########################################
###############################Single Figure################################################
ggplot(results) + geom_point(data=rand_results,aes(Intercept,-Slope),colour="darkgrey") + 
  stat_smooth(data=rand_results,aes(Intercept,-Slope),method="lm", fill="black", colour="black", size=1.5,level=0.99) +
  geom_point(data=results,aes(intercept,-slope,colour=Group),size=2.5) + scale_colour_brewer(palette="Set1")  +
  stat_smooth(data=results,aes(intercept,-slope),method="lm", fill="black", colour="red", size=1.5,level=0.99) +
  scale_y_log10('Strength of NFD',breaks=c(0.00001,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0.00001,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.text = element_text(size = 16),legend.key = element_rect(colour = NA),
        legend.position="right", legend.title = element_text(size=16),
        axis.title.y  = element_text(angle=90,size=16),
        axis.title.x  = element_text(vjust=-.25,hjust=.55,size=16),
        axis.text.y  = element_text(size=16),
        axis.text.x  = element_text(size=16))

#############################By group, with site info##########################
layout(matrix(1:6,2,3))
ggplot(birds,aes(intercept,-slope,colour = factor(site))) + geom_point(size=3) + scale_colour_hue(l=40,c=150,h=c(0,300),"Site/Community") +
  
  scale_y_log('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,0.25,1),labels=c('0','0.0001','0.001','0.01','0.1','0.25','1'),limits=c(0.00001,1.1)) +
  theme_bw() +
  opts(title="Birds", plot.title = theme_text(size=20),
       panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.text = theme_text(size = 10),legend.key = theme_rect(colour = NA, ),
       axis.title.x  = theme_text(vjust=-.25,hjust=.55,size=14))


ggplot(fish,aes(intercept,-slope,colour = factor(site))) + geom_point(size=3) + scale_colour_hue(l=40,c=150,h=c(0,300),"Site/Community") +
  
  scale_y_log('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,0.25,1),labels=c('0','0.0001','0.001','0.01','0.1','0.25','1'),limits=c(0.00001,1.1)) +
  theme_bw() +
  opts(title="Fish", plot.title = theme_text(size=20),
       panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.text = theme_text(size = 10),legend.key = theme_rect(colour = NA, ),
       axis.title.x  = theme_text(vjust=-.25,hjust=.55,size=14))


ggplot(herps,aes(intercept,-slope,colour = factor(site))) + geom_point(size=3) + scale_colour_hue(l=40,c=150,h=c(0,300),"Site/Community") +
  
  scale_y_log('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,0.25,1),labels=c('0','0.0001','0.001','0.01','0.1','0.25','1'),limits=c(0.00001,1.1)) +
  theme_bw() +
  opts(title="Herps", plot.title = theme_text(size=20),
       panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.text = theme_text(size = 10),legend.key = theme_rect(colour = NA, ),
       axis.title.x  = theme_text(vjust=-.25,hjust=.55,size=14))

ggplot(inverts,aes(intercept,-slope,colour = factor(site))) + geom_point(size=3) + scale_colour_hue(l=40,c=150,h=c(0,300),"Site/Community") +
  
  scale_y_log('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,0.25,1),labels=c('0','0.0001','0.001','0.01','0.1','0.25','1'),limits=c(0.00001,1.1)) +
  theme_bw() +
  opts(title="Invertebrates", plot.title = theme_text(size=20),
       panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.text = theme_text(size = 10),legend.key = theme_rect(colour = NA, ),
       axis.title.x  = theme_text(vjust=-.25,hjust=.55,size=14)) 


ggplot(mammals,aes(intercept,-slope,colour = factor(site))) + geom_point(size=3) + scale_colour_hue(l=40,c=150,h=c(0,300),"Site/Community") +
  
  scale_y_log('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,0.25,1),labels=c('0','0.0001','0.001','0.01','0.1','0.25','1'),limits=c(0.00001,1.1)) +
  theme_bw() +
  opts(title="Mammals", plot.title = theme_text(size=20),
       panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.text = theme_text(size = 10),legend.key = theme_rect(colour = NA, ),
       axis.title.x  = theme_text(vjust=-.25,hjust=.55,size=14)) 


ggplot(plants,aes(intercept,-slope,colour = factor(site))) + geom_point(size=3) + scale_colour_hue(l=40,c=150,h=c(0,300),"Site/Community") +
  
  scale_y_log('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,0.25,1),labels=c('0','0.0001','0.001','0.01','0.1','0.25','1'),limits=c(0.00001,1.1)) +
  theme_bw() +
  opts(title="Plants", plot.title = theme_text(size=20),
       panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.text = theme_text(size = 10),legend.key = theme_rect(colour = NA, ),
       axis.title.x  = theme_text(vjust=-.25,hjust=.55,size=14)) 

##############################Persistence Figures##############################
ggplot(results, aes(factor(Pcat), Persistence, fill=Group, colour=Pcat)) + geom_boxplot(weight=2) + facet_wrap(~Group) +
  scale_y_continuous("% Presence over time series") + scale_fill_brewer(palette="Set1") + theme_bw() +
  opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.position='none', axis.title.x = theme_blank(), axis.title.y = theme_text(angle=90,size=14),
       strip.text.x = theme_text(size = 12), axis.text.y  = theme_text(size=12),
       axis.text.x  = theme_text(size=12) ) + 
  scale_x_discrete(breaks=c("N","P"),labels=c("Ephemeral", "Persistent"))
      

ggplot(results, aes(factor(Pcat), Median+0.01, fill=Group, colour=Pcat)) + geom_boxplot() + 
  facet_wrap(~Group) + scale_y_log("Median N") + scale_fill_brewer(palette="Set1") + theme_bw() +
  opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.position='none',axis.title.x  = theme_blank(),
       axis.title.y = theme_text(angle=90,size=14), strip.text.x = theme_text(size = 12), axis.text.y  = theme_text(size=12),
       axis.text.x  = theme_text(size=12) ) + scale_x_discrete(breaks=c("N","P"),labels=c("Ephemeral", "Persistent"))
  
            #Barplot of persistent or ephemeral (in defense)
ggplot(results, aes(factor(Pcat),fill=Pcat)) + geom_bar() +  
  scale_x_discrete("",limits=c("P","N"),labels=c("Persistent","Ephemeral")) + theme_bw() + 
  scale_fill_hue(l=40, c=30, h=c(110, 360),name = "Category",breaks=c("P","N"),labels=c("Persistent", "Ephemeral")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
       axis.title.y = element_text(angle=90,size=18),
       axis.text.y  = element_text(size=16),
       axis.text.x  = element_text(size=16), 
       legend.position='none',legend.key = element_rect(colour = NA),
       legend.text = element_text(size = 12),
       axis.title.x  = element_text(vjust=-.25,hjust=.55,size=18))

                #Abundance distributions (in defense)
results2=results[!is.na(results$Pcat),]
ggplot(results2) + geom_density(data=results2,aes(Median),size=2,adjust=2,kernal="gaussian",from=0,to=300) +  
  geom_density(data=subset(results2,Pcat=="P"),aes(Median),size=2,adjust=2,kernal="gaussian",from=0,colour="purple") +
  scale_x_continuous("Median Abundance",limits=c(0,200)) + theme_bw() + 
  scale_colour_manual(name = "Community",values=c("All Species"="black","Persistent Only"="purple")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
       axis.title.y = element_text(angle=90,size=18),
       axis.text.y  = element_text(size=16),
       axis.text.x  = element_text(size=16), 
       legend.position=c(0.9,0.8),legend.key = element_rect(colour = NA),
       legend.text = element_text(size = 16),legend.title = element_text(size = 16),
       axis.title.x  = element_text(vjust=-.25,hjust=.55,size=18)) +
  annotate("text", x = 180, y = .13, label = "All Species", colour = "black",size=6) +
  annotate("text", x = 180, y = .12, label = "Persistent Only", colour = "purple",size=6) 
###############################################################################


ggplot(pattern, aes(Persistent.S, fill=Pcat)) + geom_density(alpha=0.5) +  
  scale_x_continuous("Persistent S") + theme_bw() + 
  scale_fill_hue(l=40, c=30, h=c(110, 360),name = "Community Pattern",breaks=c("N","Y"),labels=c("p > 0.1", "p < 0.1")) +
  opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
      axis.title.y = theme_text(angle=90,size=14),
       axis.text.y  = theme_text(size=12),
       axis.text.x  = theme_text(size=12), 
       legend.position=c(0.9,0.8),legend.key = theme_rect(colour = NA),
       legend.text = theme_text(size = 12),
       axis.title.x  = theme_text(vjust=-.25,hjust=.55,size=14))
      

ggplot(pattern,aes(X..increase.in.Pattern*100,fill=Pcat)) + geom_histogram(aes(y = ..count..)) + 
  
  scale_x_continuous('% difference in Covariance (cov_obs/cov_rand)') + theme_bw() +
  scale_fill_hue(l=40, c=30, h=c(110, 360),name = "Community Pattern",breaks=c("N","Y"),labels=c("p > 0.1", "p < 0.1")) +
  opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.text = theme_text(size = 12),legend.key = theme_rect(colour = NA),
       legend.position=c(0.9,0.8),
       axis.title.y  = theme_text(angle=90,size=16),
       axis.title.x  = theme_text(vjust=-.25,hjust=.55,size=16),
       axis.text.y  = theme_text(size=12),
       axis.text.x  = theme_text(size=12))

ggplot(pattern,aes(Pattern,fill=Pcat)) + geom_histogram(aes(y = ..count..)) + 
  
  scale_x_continuous('Uncorrected Covariance') + theme_bw() +
  scale_fill_hue(l=40, c=30, h=c(110, 360),name = "Community Pattern",breaks=c("N","Y"),labels=c("p > 0.1", "p < 0.1")) +
  opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.text = theme_text(size = 12),legend.key = theme_rect(colour = NA),
       legend.position=c(0.2,0.85),
       axis.title.y  = theme_text(angle=90,size=16),
       axis.title.x  = theme_text(vjust=-.25,hjust=.55,size=16),
       axis.text.y  = theme_text(size=12),
       axis.text.x  = theme_text(size=12))
  
