#################################INVERTS############################################################
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
files=c("./Invertebrates/cacoastmolluscs.txt","./Invertebrates/hubbardleps.txt",'./Invertebrates/jornadapitC.txt',
        './Invertebrates/jornadapitG.txt','./Invertebrates/jornadapitM.txt','./Invertebrates/jornadapitT.txt','./Invertebrates/luquillosnails.txt',
        './Invertebrates/oneidazooplankton.txt','./Invertebrates/pacificarthropodsCAY.txt','./Invertebrates/pacificechinoderms.txt',
        './Invertebrates/pacificmolluscsBOA.txt','./Invertebrates/pacificmolluscsCAY.txt','./Invertebrates/pacificmolluscsGPT.txt',
        './Invertebrates/pacificmolluscsHAZ.txt','./Invertebrates/pacificmolluscsMCR.txt','./Invertebrates/pacificmolluscsOCC.txt',
        './Invertebrates/pacificmolluscsPSN.txt','./Invertebrates/pacificmolluscsSAD.txt','./Invertebrates/pacificmolluscsSHB.txt',
        './Invertebrates/ukbutterfliesA.txt','./Invertebrates/ukbutterfliesC.txt','./Invertebrates/ukbutterfliesF.txt',
        './Invertebrates/ukbutterfliesG.txt','./Invertebrates/ukbutterfliesM.txt','./Invertebrates/ukbutterfliesW.txt')

names=c("CACoastlineMolluscs","HubbardBrookLeps",'JornadaPitfallsCreosote',
        'JornadaPitfallsGrassland','JornadaPitfallsMesquite','JornadaPitfallsTarbush','LuquilloSnails',
        'OneidaLakeZooplankton','PacificCoastArthropodsCAY','PacificCoastEchinoderms',
        'PacificCoastMolluscsBOA','PacificCoastMolluscsCAY','PacificCoastMolluscsGPT',
        'PacificCoastMolluscsHAZ','PacificCoastMolluscsMCR','PacificCoastMolluscsOCC',
        'PacificCoastMolluscsPSN','PacificCoastMolluscsSAD','PacificCoastMolluscsSHB',
        'UKButterfliesAgricultural','UKButterfliesCoastal','UKButterfliesForest',
        'UKButterfliesGrassland','UKButterfliesMixed','ukbutterfliesWetland')

for(i in 1:length(files)) {
  
  write_results(files[i],names[i],"invertresults.csv","invertpattern.csv")
  
}
