PlotMissingvalues = PlotMissingValues = function(Data, Names,
                                                 WhichDefineMissing = c('NA','NaN','DUMMY','.',' '),
                                                 PlotIt = TRUE,
                                                 xlab = 'Amount Of Missing Values in Percent',
                                                 xlim = c(0,100),...){
  # 
  # V = PlotMissingvalues(required variable setting)
  # V = PlotMissingvalues(require + exemplary optional variable setting)
  # 
  # DESCRIPTION
  # A one to two sentence description of what is going on.
  # 
  # INPUT
  # Data[1:n,1:d]              Numeric matrix with n cases and d observations.
  # Names[1:n]                 Numeric vector which does xyz
  # WhichDefineMissing[1:n]    Character vector which defines missing value
  #                            (Default: WhichDefineMissing =
  #                            c('NA','NaN','DUMMY','.',' '))
  # PlotIt                     Logical which decides plot output (Default: Boolean=TRUE)
  # xlab                       Optional character for plot description
  # xlim                       Optional character for plot description
  # 
  # OUTPUT
  # Object with Nans
  # 
  # 
  # Authors: Michael Thrun, Quirin Stier 10/2022
  # 
  if(is.matrix(Data)){
    Data = as.data.frame(Data)
  }
  if(!is.data.frame(Data)){
    warning('Either matrix or data.frame is expected. Calling as.data.frame()')
    Data=as.data.frame(Data)
  }

  #setting infinitive values also to nans
  Classes = apply(Data, 2, class)
  nums=which(Classes!="character")
  for(i in 1:length(nums)){
    Data[!is.finite(Data[,nums[i]]),nums[i]]=NaN
  }
  
  d=ncol(Data)
  ncases=nrow(Data)
  #casts in case of tibble
  def.par <-par(no.readonly = TRUE) # save default, for resetting...
  nas=nans=dummy=point=blank=0
  if(any(WhichDefineMissing=="NA")){
    nas=c()
    for(i in 1:d){
      temp=as.vector(as.matrix(Data[,i]))
      temp[is.nan(temp)]=0
      nas=c(nas,sum(is.na(temp)))
    }
  }
  if(any(WhichDefineMissing=="NaN")){
    nans=c()
    for(i in 1:d){
      nans=c(nans,sum(is.nan(as.vector(as.matrix(Data[,i])))))
    }
  }
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
  if(missing(Names) || is.null(Names)){
    if(!is.null(colnames(Data))){
      Names = colnames(Data)
    }else{
      Names = paste0('C',1:d)
    } 
  }
  nana = nas + nans + dummy + point + blank
  names(nana) = Names
  par(mar = c(5, 18, 1, 1) + 0.2)
  options(repr.plot.width=4, repr.plot.height=8)
  par(las=2) 
  #barplot((nana/ncases)*100, horiz = T, xlab = xlab, xlim = xlim)
  barplot((nana/ncases)*100, horiz = T, xlab = xlab, xlim = xlim, ...)
  par(def.par)
  return(invisible(nana))
}
