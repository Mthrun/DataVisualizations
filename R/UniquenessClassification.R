UniquenessClassification=function(DataFrameOrMatrix,UniqueIdentifier,UniqueClass=0,Summary=T){
n=nrow(DataFrameOrMatrix)
d=ncol(DataFrameOrMatrix)
  cols=colnames(DataFrameOrMatrix)
  id=which(cols==UniqueIdentifier)
  if(length(id)==1){

#incaseof tibble
u=unique(as.vector(as.matrix(DataFrameOrMatrix[,id])))

Classification=rep(NaN,n)
for(i in 1:length(u)){

  ind=which(DataFrameOrMatrix[,id]==u[i])
  if(length(ind)==1){
    Classification[ind]=UniqueClass
  }else{
    Classification[ind]=1:length(ind)
  }
}

Classification[!is.finite(Classification)]=9999999

if(isTRUE(Summary)){
  V=ClassCount(Classification)
  print(V[1:4])
}
return(Classification)
  }else{
    return(NULL)
  }
}