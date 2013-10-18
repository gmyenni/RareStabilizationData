library('ggplot2')

results=read.csv("./AllResults.csv",header=T)
results$Pcat[which(is.na(results$intercept))]=NA

pattern=read.csv("./AllPattern.csv",header=T)
pattern$Pcat[which(pattern$p.val<0.1)]="Y"
pattern$Pcat[which(pattern$p.val>0.1)]="N"
pattern$Srat=pattern$Persistent.S/pattern$Observed.S
pattern$Pdiff=pattern$Pattern-pattern$Null.Pattern
sig.list=pattern$Site[which(pattern$Pcat=="Y")]
nonsig.list=pattern$Site[which(pattern$Pcat=="N")]

birds <- subset(results, Group == "Birds")
fish <- subset(results, Group == "Fish")
herps <- subset(results, Group == "Herps")
inverts <- subset(results, Group == "Invertebrates")
mammals <- subset(results, Group == "Mammals")
plants <- subset(results, Group == "Plants")

percents=data.frame(Group=c("Birds","Fish","Herps","Invertebrates","Mammals","Plants"),per=c("0.5","0.5","0.09","0.44","0.53","0.53"))

rand_results=read.csv("background.csv",header=T)

library("fdrtool")
q.val=fdrtool(pattern$p.val, statistic="pvalue")$qval
pattern$q.val=q.val
###############################Single Figure, by group###################################
ggplot(results,aes(intercept,-slope,colour=Group)) + geom_point() + scale_colour_brewer(palette="Set1")  +
  
  scale_y_log('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  
  facet_wrap(~Group) + geom_text(data=percents, aes(x=0.6, y=20000, label=per), parse=TRUE, colour="black", size=6) + theme_bw() +
  opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.position='none',axis.title.x  = theme_text(vjust=-.25,hjust=.54,size=16),
       axis.title.y  = theme_text(angle=90,size=16), 
       strip.text.x = theme_text(size = 14),
       axis.text.y  = theme_text(size=11),
       axis.text.x  = theme_text(size=11))
###############################Single Figure, by group, with lm's###################################
ggplot(results) + geom_point(data=rand_results,aes(Intercept,-Slope),colour="darkgrey") + 
  stat_smooth(data=rand_results,aes(Intercept,-Slope),method="lm", fill="black", colour="black", size=2) +
  geom_point(data=results,aes(intercept,-slope,colour=Group)) + scale_colour_brewer(palette="Set1")  +
  stat_smooth(data=results,aes(intercept,-slope,colour=Group),method="lm", fill="black", size=2) +
  scale_y_log10('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  facet_wrap(~Group) + geom_text(data=percents, aes(x=0.6, y=20000, label=per), parse=TRUE, colour="black", size=6) + theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        legend.text = element_text(size = 16),legend.key = element_rect(size=4,colour = NA, ),
        legend.position="right", legend.title = element_text(size=16),
        axis.title.y  = element_text(angle=90,size=16),
        axis.title.x  = element_text(vjust=-.25,hjust=.55,size=16),
        axis.text.y  = element_text(size=16),
        axis.text.x  = element_text(size=16))

###############################Single Figure################################################
ggplot(results) + geom_point(data=rand_results,aes(Intercept,-Slope),colour="darkgrey") + 
  stat_smooth(data=rand_results,aes(Intercept,-Slope),method="lm", fill="black", colour="black", size=1.5,level=0.99) +
  geom_point(data=results,aes(intercept,-slope,colour=Group),size=2.5) + scale_colour_brewer(palette="Set1")  +
  stat_smooth(data=results,aes(intercept,-slope),method="lm", fill="black", colour="red", size=1.5,level=0.99) +
  scale_y_log10('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
       legend.text = element_text(size = 16),legend.key = element_rect(colour = NA),
       legend.position="right", legend.title = element_text(size=16),
       axis.title.y  = element_text(angle=90,size=16),
       axis.title.x  = element_text(vjust=-.25,hjust=.55,size=16),
       axis.text.y  = element_text(size=16),
       axis.text.x  = element_text(size=16))

################################Figure 2 (two separate figures)###########################
ggplot(results) +  
  stat_smooth(data=rand_results,aes(Intercept,-Slope),method="lm", fill="black", colour="black", size=1.5,level=0.99,alpha=0.8,linetype = 2) +
  geom_point(data=results,aes(intercept,-slope,colour=Group),size=2.5) + scale_colour_brewer(palette="Set1")  +
  stat_smooth(data=results,aes(intercept,-slope),method="lm", fill="black", colour="black", size=1.5,level=0.99,alpha=0.8) +
  scale_y_log10('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000'),limits=c(0.1,100000)) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
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
  scale_y_log10('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000'),limits=c(0.1,100000)) + 
  scale_x_log10('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
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
  
