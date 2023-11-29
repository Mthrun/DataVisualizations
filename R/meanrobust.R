Meanrobust=meanrobust=function(x, p=0.1,na.rm=TRUE){

  if(is.matrix(x)){
    mhat<-c()
    for(i in 1:dim(x)[2]){
       mhat[i]<-mean(x[,i],trim=p,na.rm=na.rm)
    }
  } else  mhat<-mean(x,trim=p,na.rm=na.rm) 
 
 return (mhat) 

 }

