#Combine all results

###############################MAMMALS##############################################################


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
files=c("./Mammals/Portal/rodents_controls.txt","./Mammals/sevilleta/5pgrass.txt",
        "./Mammals/sevilleta/5plarrea.txt","./Mammals/sevilleta/rslarrea.txt",
        "./Mammals/Konza_Abund.txt","./Mammals/curlew.txt","./Mammals/INEEL.txt",
        "./Mammals/Jornadagrass.txt","./Mammals/Jornadashrub.txt","./Mammals/Powdermill/pdm_sqabund.txt",
        "./Mammals/Powdermill/pdm_rodabund.txt","./Mammals/shortgrass.txt","./Mammals/Ontario_rod.txt",
        "./Mammals/hilaire.txt","./Mammals/karoo_ungulates.txt","./Mammals/KrugerNP.txt",
        "./Mammals/goldengate.txt")
names=c("Portal","Sev5pgrass","Sev5plarrea","Sevrslarrea","Konza","Curlew","INEEL","JornadaGrass",
        "JornadaShrub","Powdermillsq","Powdermillrod","Shortgrass","Ontario","Hilaire","KarooNP",
        "KrugerNP","GoldengateNP")

for(i in 1:length(files)) {

write_results(files[i],names[i],"mammalresults.csv","mammalpattern.csv")

}
