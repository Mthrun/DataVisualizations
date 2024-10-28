Meanrobust=function(x, p=10,na.rm=TRUE){
  if(p>=1){
    p=p/100
  }
  if(is.matrix(x)){
    mhat<-c()
    for(i in 1:dim(x)[2]){
       mhat[i]<-mean(x[,i],trim=p,na.rm=na.rm)
    }
  } else  mhat<-mean(x,trim=p,na.rm=na.rm) 
 
 return (mhat) 

 }

