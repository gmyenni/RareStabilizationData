library('ggplot2')

results=read.csv("./AllResults.csv",header=T)

pattern=read.csv("./AllPattern.csv",header=T)
pattern$Pcat[which(pattern$p.val<0.1)]="Y"
pattern$Pcat[which(pattern$p.val>0.1)]="N"
pattern$Srat=pattern$Persistent.S/pattern$Observed.S
pattern$Pdiff=pattern$Pattern-pattern$Null.Pattern
sig.list=pattern$Site[which(pattern$Pcat=="Y")]
nonsig.list=pattern$Site[which(pattern$Pcat=="N")]
results$Pcat=NA

for(i in 1:length(sig.list)){ 
  site=sig.list[i]
  results$Pcat[which(results$site==site)]=="Y"}

birds <- subset(results, Group == "Birds")
fish <- subset(results, Group == "Fish")
herps <- subset(results, Group == "Herps")
inverts <- subset(results, Group == "Invertebrates")
mammals <- subset(results, Group == "Mammals")
plants <- subset(results, Group == "Plants")

percents=data.frame(Group=c("Birds","Fish","Herps","Invertebrates","Mammals","Plants"),per=c("0.5","0.5","0.09","0.44","0.53","0.53"))

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

###############################Single Figure################################################
ggplot(results,aes(intercept,-slope,colour=Group)) + geom_point() + scale_colour_brewer(palette="Set1")  +
  
  scale_y_log('Strength of NFD',breaks=c(0,1,10,100,1000,10000),labels=c('0','1','10','100','1000','10000')) + 
  scale_x_log('Equilibrium frequency',breaks=c(0,0.0001,0.001,0.01,0.1,1),labels=c('0','0.0001','0.001','0.01','0.1','1'),limits=c(0.00001,1.1)) +
  theme_bw() +
  opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.text = theme_text(size = 12),legend.key = theme_rect(colour = NA, ),
       legend.position="right",
       axis.title.y  = theme_text(angle=90,size=16),
       axis.title.x  = theme_text(vjust=-.25,hjust=.55,size=16),
       axis.text.y  = theme_text(size=12),
       axis.text.x  = theme_text(size=12))



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
  
