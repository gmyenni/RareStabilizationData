#############################Calculate RAs from abundance data##############################################

RA=function(filename, sitename, resultsfile) { 
  
  #Read file
  abund=read.table(filename,header=T)
  species=colnames(abund[4:dim(abund)[2]])
  #Run analysis
  ra=abund[,4:dim(abund)[2]]/abund[,3]
  ra[ra < 0] <- NA
  ra_out=apply(ra,2,mean,na.rm=T)
  
  #Save results  
  write.table(cbind(sitename,species,ra_out), resultsfile, append=T, sep=",", col.names = F, row.names=FALSE)
  
}

#List of files
files=c('./Plants/desertlab.txt', './Plants/desertlabshrub.txt', './Plants/Jornada_CS.txt',
        './Plants/Jornada_CW.txt', './Plants/Jornada_GS.txt', './Plants/Jornada_GW.txt',
        './Plants/Jornada_MS.txt', './Plants/Jornada_MW.txt', './Plants/Jornada_PS.txt',
        './Plants/Jornada_PW.txt', './Plants/Jornada_TS.txt', './Plants/Jornada_TW.txt',      
        './Plants/kansasannuals.txt', './Plants/kansasperennials.txt', './Plants/portalsummer.txt', 
        './Plants/portalwinter.txt', './Plants/steppe_counts.txt')
names=c('DesertLabOpen', 'DesertLabShrub', 'JornadaCS', 'JornadaCW',     
        'JornadaGS', 'JornadaGW', 'JornadaMS', 'JornadaMW',      
        'JornadaPS', 'JornadaPW', 'JornadaTS', 'JornadaTW',      
        'KansasAnnuals', 'KansasPerennials', 'PortalSummer',
        'PortalWinter', 'Steppe')

for(i in 1:length(files)) {
  
  RA(files[i],names[i],"plantras.csv")
  
}
