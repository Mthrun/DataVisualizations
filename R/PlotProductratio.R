PlotProductratio=ProductRatioPlot=function(X,Y,na.rm=FALSE,main='Product Ratio Analysis',xlab='Log of Ratio',ylab='Root of Product',...){
  
  if(length(X)!=length(Y)) stop('Vectors X and Y have to be of equal length')
  
  if(isTRUE(na.rm)){ #achtung irgendwas stimmt hier nicht
    noNaNInd <- which(is.finite(X)&is.finite(Y))
    X <- X[noNaNInd]
    Y <- Y[noNaNInd]
  }
  
  positive=which(X>=0 & Y>=0)
  X=X[positive]
  Y=Y[positive]
  
  Product=sqrt(X*Y)
  logs=log(X/Y)
  plot(logs,Product,xlab=xlab,ylab=ylab,main=main,...)
  
  return(invisible(cbind(Product,logs)))
}