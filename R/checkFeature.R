checkFeature=function(x,varname='x',Funname=""){
  #achtung deparse1(substitute(X))
  #funktioniert nach aufruf dieser funktion nichtmehr
  if(missing(x)){
    stop(paste0("Variable is missing in function '",Funname,"'."))
  }
    
  if(!is.vector(x)){
    warning(paste(varname,'is not a vector. calling as.vector()'))
    x=as.vector(x)
  }
  #returns(M (log ratio) and A (mean average) scales)
  if(mode(x)!='numeric'){
    warning(paste(varname,'is not a numeric vector, calling as.numeric()'))
    x=as.numeric(x)
  }
  if(sum(!is.finite(x))) warning(paste('Not finite values in',varname))
  if(length(unique(x))<3){
    warning(paste('Only two or less unique values in vector',varname,'- called method may not work properly.'))
  }
  return(x)
}