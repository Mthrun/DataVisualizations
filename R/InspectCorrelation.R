InspectCorrelation=function(x,y,na.rm=FALSE,paretoRadius=0,sampleSize=round(sqrt(500000000),-3),
                            
                            NrOfContourLines=20,Plotter='native', DrawTopView = T,
                            
                            xlab="X", ylab="Y", main='Spearman correlation coef.:',
                            
                            xlim, ylim, Legendlab_ggplot="value"){
  x=checkFeature(x,'x')
  y=checkFeature(y,'y')
  if(identical(x,y)){
    stop('Variable x is identical to variable y. Please check input.')
  }
  if(isTRUE(na.rm)){ #achtung irgendwas stimmt hier nicht
    noNaNInd <- which(is.finite(x)&is.finite(y))
    x <- x[noNaNInd]
    y <- y[noNaNInd]
  }
  ccc=round(cor(x,y,method = 'spearman'),2)
  
  main=paste(main,ccc)
  return(PDEscatter(x,y,na.rm=FALSE,paretoRadius=paretoRadius,sampleSize=sampleSize,
             
             NrOfContourLines=NrOfContourLines,Plotter=Plotter, DrawTopView = DrawTopView,
             
             xlab=xlab, ylab=ylab, main=main,
             
             xlim=xlim, ylim=ylim, Legendlab_ggplot=Legendlab_ggplot))
  
}