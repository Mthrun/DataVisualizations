MDplot4multiplevectors=function(...,Names, Ordering='Default',Scaling="None",Fill='darkblue',
                       RobustGaussian=TRUE,GaussianColor='magenta',Gaussian_lwd=1.5,
                       BoxPlot=FALSE,BoxColor='darkred',MDscaling='width',Size=0.01,
                       MinimalAmoutOfData=40,OnlyPlotOutput=TRUE){
  requireNamespace('rowr')
  df=rowr::cbind.fill(...,fill=NaN)
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
         MinimalAmoutOfData=MinimalAmoutOfData,OnlyPlotOutput=OnlyPlotOutput)
}