checkFeature=function(x,f='x'){
  
  if(!is.vector(x)){
    warning(paste(f,'is not a vector. calling as.vector()'))
    x=as.vector(x)
  }
  #returns(M (log ratio) and A (mean average) scales)
  if(mode(x)!='numeric'){
    warning(paste(f,'is not a numeric vector, calling as.numeric()'))
    x=as.numeric(x)
  }
  if(sum(!is.finite(x))) warning(paste('Not finite values in',f))
  if(length(unique(x))<3){
    warning(paste('Only two or less unique values in vector',f,'- called method may not work properly.'))
  }
  return(x)
}