InspectCorrelation=function(x,y,DensityEstimation="SDH",CorMethod='spearman',na.rm=TRUE,SampleSize=round(sqrt(500000000),-3),
                            
                            NrOfContourLines=20,Plotter='native', DrawTopView = T,
                            
                            xlab="X", ylab="Y", main='Spearman correlation coef.:',
                            
                            xlim, ylim, Legendlab_ggplot="value",...){
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
  ccc=round(cor(x,y,method = CorMethod),2)
  
  main=paste(main,ccc)
ggobject=DensityScatter(x,y,DensityEstimation=DensityEstimation,na.rm=FALSE,SampleSize =SampleSize,
             
             NrOfContourLines=NrOfContourLines,Plotter='ggplot', DrawTopView = TRUE,
             
             xlab=xlab, ylab=ylab, main=main,
             
             xlim=xlim, ylim=ylim, Legendlab_ggplot=Legendlab_ggplot,...)$Handle
#print(ggobject)
#ggExtra::ggMarginal(ggobject, type="histogram", bins = 25,x='x',y='y')
return(ggobject)  
}