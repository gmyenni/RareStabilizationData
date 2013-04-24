library('ggplot2')

pattern=read.csv("./AllPattern.csv",header=T)
dissim=read.csv("./dissimilarity.csv",header=T)
dissim$sign[which(dissim$pval<=0.05)]="Y"
dissim$sign[which(dissim$pval>0.05)]="N"
pattern$Pcat[which(pattern$p.val<0.1)]="Y"
pattern$Pcat[which(pattern$p.val>0.1)]="N"
pattern$Pdiff=pattern$Pattern-pattern$Null.Pattern
dissim$pattern=pattern$Pdiff
dissim$Pcat=pattern$Pcat


###little function to count sample size for figures###

give.n <- function(x){
  return(c(y = 0.99, label = length(x)))
}

ggplot(dissim, aes(factor(Pcat), RankCorr)) + geom_boxplot() + facet_wrap(~Type) +
  opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
       legend.position='none',axis.title.x  = theme_text(vjust=-.25,hjust=.54,size=16),
       axis.title.y  = theme_text(angle=90,size=16), 
       strip.text.x = theme_text(size = 14),
       axis.text.y  = theme_text(size=11),
       axis.text.x  = theme_text(size=11))


ggplot(dissim, aes(factor(sign), pattern)) + geom_boxplot() + facet_wrap(~Type)
