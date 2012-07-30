#################################BIRDS##############################################################

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
files=c("./Birds/hubbard.txt","./Birds/whitemountain.txt","./Birds/redvers.txt",
        "./Birds/skokholm.txt","./Birds/konza_waterfowl.txt","./Birds/konza_songbirds.txt",
        "./Birds/texas.txt","./Birds/easternwood.txt","./Birds/pawnee_birds.txt",
        "./Birds/pawnee_raptors.txt","./Birds/mountainbird_maine.txt","./Birds/mountainbird_ny.txt",
        "./Birds/mountainbird_vt.txt","./Birds/elverde.txt")
names=c("Hubbard","WhiteMountain","Redvers","Skokholm","KonzaWaterfowl","KonzaSongbirds",
        "Texas","EasternWood","Pawnee","PawneeRaptors","Maine","NewYork","GreenMountains",
        "Luquillo")

for(i in 1:length(files)) {
  
  write_results(files[i],names[i],"birdresults1.csv","birdpattern1.csv")
  
}
