stdrobust <- function(x,lowInnerPercentile=25){
  prctile=function (x, p) 
  {
    if (length(p) == 1) {
      if (p > 1) {
        p = p/100
      }
    }
    if (is.matrix(x) & ncol(x) > 1) {
      cols <- ncol(x)
      quants <- matrix(0, nrow = length(p), ncol = cols)
      for (i in 1:cols) {
        quants[, i] <- quantile(x[, i], probs = p, type = 5, 
                                na.rm = TRUE)
      }
    }
    else {
      quants <- quantile(x, p, type = 5, na.rm = TRUE)
    }
    return(quants)
  }
  
  if(is.vector(x) | (is.matrix(x) & dim(x)[1]==1)) dim(x)<-c(length(x),1)
  
  lowInnerPercentile<-max(1,min(lowInnerPercentile,49))
  hiInnerPercentile<- 100 - lowInnerPercentile
  #norminv=qnorm
  faktor<-sum(abs(qnorm(c(lowInnerPercentile,hiInnerPercentile)/100,0,1)))
  std<-sd(x,na.rm=TRUE)
 
  quartile<-prctile(x,c(lowInnerPercentile,hiInnerPercentile))  
  if (ncol(x)>1)
    iqr<-quartile[2,]-quartile[1,]
  else
    iqr<-quartile[2]-quartile[1]
  
  shat<-c()
  for(i in 1:ncol(x)){
      shat[i]<-min(c(std[i],iqr[i]/faktor),na.rm=TRUE)
  }
  dim(shat)<-c(1,ncol(x))
  colnames(shat)<-colnames(x)
  return (shat) 

 }

