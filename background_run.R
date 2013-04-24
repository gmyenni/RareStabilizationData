source('./background_function.R')

write_results=function(filename) { 
  
  sitefile=read.table(filename,header=T)
  
  #Run analysis
  sitename_out=background(sitefile)
  
  
  #Save results  
  write.table(cbind(rep(sub(".txt",replacement="",x=filename),dim(sitename_out)[1]),sitename_out)
              , "background.csv", append=T, sep=",", col.names = F, row.names=F)
  
}

#List of files
files=list.files(pattern=".*.txt",recursive=T,include.dirs=T)

lapply(files[-20],write_results)