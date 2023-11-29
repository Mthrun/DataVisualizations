Stdrobust =stdrobust= function(x,lowInnerPercentile=25,na.rm=TRUE){
  prctile=function (x, p,na.rm=FALSE) 
  {
    if (length(p) == 1) {
      if (p > 1) {
        p = p/100
      }
    }
  if(is.vector(x)){
    if(isTRUE(na.rm)){
        y=x
        y=y[is.finite(y)]
      }else{
        y=x
      }
      quants <- quantile(x, p, type = 5, na.rm = na.rm)
  }else{
      cols <- ncol(x)
      quants <- matrix(0, nrow = length(p), ncol = cols)
      for (i in 1:cols) {
        if(isTRUE(na.rm)){
          y=x[, i]
          y=y[is.finite(y)]
        }else{
          y=x[, i]
        }
        quants[, i] <- quantile(y, probs = p, type = 5, 
                                na.rm = na.rm)
      }
    }
    return(quants)
  }
  
  lowInnerPercentile<-max(1,min(lowInnerPercentile,49))
  hiInnerPercentile<- 100 - lowInnerPercentile
  #norminv=qnorm
  faktor<-sum(abs(qnorm(c(lowInnerPercentile,hiInnerPercentile)/100,0,1)))
 
  if(isTRUE(na.rm)){
    if(is.vector(x))
      x=x[is.finite(x)]
    else
      x=x[complete.cases(x),]
  }
  std<-sd(x,na.rm=na.rm)
 
  quartile<-prctile(x,c(lowInnerPercentile,hiInnerPercentile)/100,na.rm=na.rm)  
  if(is.vector(x)){

    iqr<-quartile[2]-quartile[1]
    shat<-min(c(std,iqr/faktor),na.rm=na.rm)
  }else{
    iqr<-quartile[2,]-quartile[1,]
    shat<-c()
    for(i in 1:ncol(x)){
      shat[i]<-min(c(std[i],iqr[i]/faktor),na.rm=na.rm)
    }
    names(shat)<-colnames(x)
  }
  

  return (shat) 

 }

