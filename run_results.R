#Combine all results

############################################################################################
ptm <- proc.time(); print(ptm)

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
files=list.files(pattern=".txt",recursive=T)[60:102]
names=gsub("\\..*","",list.files(pattern=".txt",recursive=T))[60:102]

for(i in 1:length(files)) {

write_results(files[i],names[i],"nonlinearresults.csv","nonlinearpattern.csv")

}

print(proc.time() - ptm)
