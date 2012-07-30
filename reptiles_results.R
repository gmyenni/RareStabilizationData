#################################REPTILES############################################################
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
files=c("./Herps/luquillo.txt", "./Herps/boldparksnakes.txt", "./Herps/boldparklizards.txt", 
        "./Herps/cowleysnakes.txt", "./Herps/cowleylizards.txt", "./Herps/orabandasnakes.txt",
        "./Herps/orabandalizards.txt", "./Herps/fitchsnakes.txt", "./Herps/coweetasalamanders.txt",
        "./Herps/esgeorgeturtles.txt", "./Herps/rainbowfrogs.txt")
names=c("Luquillo", "BoldParkSnakes", "BoldParkLizards", "CowleyCountySnakes", 
        "CowleyCountyLizards", "OraBandaSnakes", "OraBandaLizards", "FitchSnakes", 
        "CoweetaSalamanders", "ESGeorgeTurtles", "RainbowBayFrogs")

for(i in 1:length(files)) {
  
  write_results(files[i],names[i],"herpresults.csv","herppattern.csv")
  
}
