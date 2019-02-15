MDplot4multiplevectors=function(...,Names, Ordering='Default',Scaling="None",Fill='darkblue',
                       RobustGaussian=TRUE,GaussianColor='magenta',Gaussian_lwd=1.5,
                       BoxPlot=FALSE,BoxColor='darkred',MDscaling='width',Size=0.01,
                       MinimalAmoutOfData=40,MinimalAmoutOfUniqueData=12,SampleSize=5e+05,OnlyPlotOutput=TRUE){
  
  requireNamespace('rowr')
  inputs <- list(...)
  addcol=function(...) {return(rowr::cbind.fill(...,fill=NaN))}
  
  if(length(inputs)==1){
    if(is.list(inputs[[1]])){#the list of input consists exactly of one list
      df=do.call(what = addcol,inputs[[1]])#for every element of this list do
    }else{#assumption: only one element ist given which is not a list, error catching if its not the case is done in base
      df=as.matrix(inputs[[1]])
    }  
  }else{#several vectors are given, error catching if its not the case is done in rowr
    df=addcol(...)
  }

  if(!missing(Names)){
    if(length(Names)==ncol(df)){
      colnames(df)=Names
    }else{
      warning('Length of Names is not equal to Number of vectors. Ignoring names.')
      colnames(df)=paste0('C',1:ncol(df))
    }
    
  }else{
    colnames(df)=paste0('C',1:ncol(df))
  }

  MDplot(Data=as.matrix(df),Ordering=Ordering,Scaling=Scaling,Fill=Fill,
                  RobustGaussian=RobustGaussian,GaussianColor=GaussianColor,Gaussian_lwd=Gaussian_lwd,
                  BoxPlot=BoxPlot,BoxColor=BoxColor,MDscaling=MDscaling,Size=Size,
                  MinimalAmoutOfData=MinimalAmoutOfData,MinimalAmoutOfUniqueData=MinimalAmoutOfUniqueData,SampleSize=SampleSize,OnlyPlotOutput=OnlyPlotOutput)
}