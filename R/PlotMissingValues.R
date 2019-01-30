PlotMissingvalues=PlotMissingValues=function(Data,Names,WhichDefineMissing=c('NA','DUMMY','.',' '),PlotIt=TRUE,xlab='Amount Of Missing Values in percent',...){
  if(is.matrix(Data)){
    Data=as.data.frame(Data)
  }
  if(!is.data.frame(Data)){
    warning('Either matrix or data.frame is expected. Calling as.data.frame()')
    Data=as.data.frame(Data)
  }

  #setting infinitive values also to nans
  Classes=sapply(Data, class)
  nums=which(Classes!="character")
  for(i in 1:length(nums))
    Data[!is.finite(Data[,nums[i]]),nums[i]]=NaN
  
  d=ncol(Data)
  ncases=nrow(Data)
  #casts in case of tibble
  def.par <-
    par(no.readonly = TRUE) # save default, for resetting...
  nas=nans=dummy=point=blank=0
  if(any(WhichDefineMissing=="NA")){
    nas=c()
    for(i in 1:d){
      nas=c(nas,sum(is.na(as.vector(as.matrix(Data[,i])))))
    }
  }
  # if(any(WhichDefineMissing=="NaN")){
    nans=c()
    for(i in 1:d){
      nans=c(nans,sum(is.nan(as.vector(as.matrix(Data[,i])))))
    }
  # }
  if(any(WhichDefineMissing=="DUMMY")){
    dummy=c()
    for(i in 1:d){
      f1=sum(as.vector(as.matrix(Data[,i]))=="DUMMY",na.rm = T)
      f2=sum(as.vector(as.matrix(Data[,i]))=="Dummy",na.rm = T)
      f3=sum(as.vector(as.matrix(Data[,i]))=="dummy",na.rm = T)
      dummy=c(dummy,f2+f1+f3)
    }
  }
  if(any(WhichDefineMissing==".")){
    point=c()
    for(i in 1:d){
      point=c(point,sum(as.vector(as.matrix(Data[,i]))==".",na.rm = T))
    }
  }
  if(any(WhichDefineMissing==" ")){
    blank=c()
    for(i in 1:d){
      blank=c(blank,sum(as.vector(as.matrix(Data[,i]))==" ",na.rm = T))
    }
  }
  if(missing(Names)&!is.null(colnames(Data))){
    Names=colnames(Data)
  }else{
    Names=paste0('C',1:d)
  } 
  nana=nas+nans+dummy+point+blank
  names(nana)=Names
  par(mar = c(5, 18, 1, 1) + 0.2)
  options(repr.plot.width=4, repr.plot.height=8)
  par(las=2) 
  barplot(nana/ncases*100,horiz = T,xlab=xlab,...)
  par(def.par)
  return(invisible(nana))
}