PlotMissingValues=function(Dataframe,WhichMissing=c('NA','NaN','DUMMY','.',' '),PlotIt=TRUE,...){
  d=ncol(Dataframe)
  ncases=nrow(Dataframe)
  #casts in case of tibble
  def.par <-
    par(no.readonly = TRUE) # save default, for resetting...
  nas=nans=dummy=point=blank=0
  if(any(WhichMissing=="NA")){
    nas=c()
    for(i in 1:d){
      nas=c(nas,sum(is.na(as.vector(as.matrix(Dataframe[,i])))))
    }
  }
  if(any(WhichMissing=="NaN")){
    nans=c()
    for(i in 1:d){
      nans=c(nans,sum(is.nan(as.vector(as.matrix(Dataframe[,i])))))
    }
  }
  if(any(WhichMissing=="DUMMY")){
    dummy=c()
    for(i in 1:d){
      dummy=c(dummy,sum(as.vector(as.matrix(Dataframe[,i]))=="DUMMY",na.rm = T))
    }
  }
  if(any(WhichMissing==".")){
    point=c()
    for(i in 1:d){
      point=c(point,sum(as.vector(as.matrix(Dataframe[,i]))==".",na.rm = T))
    }
  }
  if(any(WhichMissing==" ")){
    blank=c()
    for(i in 1:d){
      blank=c(blank,sum(as.vector(as.matrix(Dataframe[,i]))==" ",na.rm = T))
    }
  }

  nana=nas+nans+dummy+point+blank
  names(nana)=colnames(Dataframe)
  par(mar = c(5, 18, 2, 2) + 0.2)
  par(las=2) 
  barplot(nana/ncases*100,horiz = T,...)
  par(def.par)
  return(invisible(nana))
}