# Ecological distance (dissimilarity), using
# Euclidean distance
dissim=function(dataset,name) {

ED = function(x,y){
  if(length(x)!=length(y)) stop("Abundance vectors of unequal length")
  out =  sqrt(sum((x-y)^2))
  out
}

# Aggregate plots
dataset[dataset<0] = 0
dataset2 = aggregate(dataset[,3:dim(dataset)[2]],by=list(Year=dataset[,1]),FUN=mean)
dataset2[,2]=rowSums(dataset2[,3:dim(dataset2)[2]])
dataset2$Year = as.numeric(as.character(dataset2$Year))

# Create a dissimilarity matrix for all year combinations and a
#"time lag" matrix, caculate Kendall rank correlation to determine
#change over time

EDmatrix = matrix(NA,dim(dataset2)[1],dim(dataset2)[1])
YRmatrix = matrix(NA,dim(dataset2)[1],dim(dataset2)[1])
for(j in 1:dim(dataset2)[1]){
  for(k in 1:dim(dataset2)[1]){
    EDmatrix[j,k] = ED(dataset2[j,4:NCOL(dataset2)], dataset2[k,4:NCOL(dataset2)]  )
    YRmatrix[j,k] =  abs(dataset2$Year[j]-dataset2$Year[k])
  }
}
EDmatrix[lower.tri(EDmatrix, diag=T)]=NA
YRmatrix[lower.tri(YRmatrix, diag=T)]=NA
y = as.vector(EDmatrix)
x = as.vector(YRmatrix)

test=cor.test(x,y,method="kendall") 
parcel=list(name,test$estimate,test$p.value)
return(parcel)

}