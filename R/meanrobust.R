meanrobust <- function(x, p=0.1){

  if(is.matrix(x)){
    mhat<-c()
    for(i in 1:dim(x)[2]){
       mhat[i]<-mean(x[,i],trim=p,na.rm=TRUE)
    }
  } else  mhat<-mean(x,trim=p,na.rm=TRUE) 
 
 return (mhat) 

 }

