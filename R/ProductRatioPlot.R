ProductRatioPlot=function(x,y,na.rm=FALSE,main='Product Ratio Analysis',xlab='Log of Ratio',ylab='Root of Product',...){
  
  if(length(x)!=length(y)) stop('Vectors x and y have to be of equal length')
  
  if(isTRUE(na.rm)){ #achtung irgendwas stimmt hier nicht
    noNaNInd <- which(is.finite(x)&is.finite(y))
    x <- x[noNaNInd]
    y <- y[noNaNInd]
  }

  positive=which(x>=0 & y>=0)
  x=x[positive]
  y=y[positive]
  
  Product=sqrt(x*y)
  logs=log(x/y)
  plot(logs,Product,xlab=xlab,ylab=ylab,main=main,...)
  
  return(invisible(cbind(Product,logs)))
}