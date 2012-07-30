#################################FISH############################################################
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
files=c("./Fish/hinkleyflatfish.txt" ,"./Fish/hinkleygadoidfish.txt" ,"./Fish/northseademersal.txt",
        "./Fish/northseaflatfish.txt", "./Fish/northseagadoid.txt", "./Fish/northseapelagic.txt")
names=c("HinkleyFlatfish" ,"HinkleyGadoidfish" ,"NorthseaDemersal",
        "NorthseaFlatfish", "NorthseaGadoid", "NorthseaPelagic")

for(i in 1:length(files)) {
  
  write_results(files[i],names[i],"fishresults.csv","fishpattern.csv")
  
}

