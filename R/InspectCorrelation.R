InspectCorrelation=function(x,y,na.rm=FALSE,paretoRadius=0,sampleSize=round(sqrt(500000000),-3),
                            
                            NrOfContourLines=20,Plotter='native', DrawTopView = T,
                            
                            xlab="X", ylab="Y", main='Spearman correlation coef.:',
                            
                            xlim, ylim, Legendlab_ggplot="value"){
  x=checkFeature(x,'x')
  y=checkFeature(y,'y')
  #requireNamespace('ggExtra')
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
ggobject=PDEscatter(x,y,na.rm=FALSE,paretoRadius=paretoRadius,sampleSize=sampleSize,
             
             NrOfContourLines=NrOfContourLines,Plotter='ggplot', DrawTopView = TRUE,
             
             xlab=xlab, ylab=ylab, main=main,
             
             xlim=xlim, ylim=ylim, Legendlab_ggplot=Legendlab_ggplot)$Handle
#print(ggobject)
#ggExtra::ggMarginal(ggobject, type="histogram", bins = 25,x='x',y='y')
return(ggobject)  
}