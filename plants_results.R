#################################PLANTS##############################################################

write_results=function(filename, sitename, resultsfile, patternfile) { 
  
  source("./stabil_function.r")
  
  #Read file
  sitefile=read.table(filename,header=T)
  
  #Run analysis
  sitename_out=stabil(sitefile,names(sitefile)[-(1:3)],sitename)
  
  #Save results  
  write.table(cbind(site=sitename,sitename_out$results), resultsfile, append=T, sep=",", col.names = F, row.names=FALSE)
  write.table(cbind(site=sitename,sitename_out$pattern), patternfile, append=T, sep=",", col.names = F, row.names=FALSE)
  
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
  
  write_results(files[i],names[i],"plantresults.csv","plantpattern.csv")
  
}


