InspectCorrelation=function(X,Y,DensityEstimation="SDH",CorMethod='spearman',na.rm=TRUE,SampleSize=round(sqrt(500000000),-3),
                            
                            NrOfContourLines=20,Plotter='native', DrawTopView = T,
                            
                            xlab, ylab, main='Spearman correlation coef.:',
                            
                            xlim, ylim, Legendlab_ggplot="value",...){
  
  if(missing(xlab)) xlab=deparse1(substitute(X))
  if(missing(ylab)) ylab=deparse1(substitute(Y))
  
  X=checkFeature(X,'X')
  Y=checkFeature(Y,'Y')
  #requireNamespace('ggExtra')
  if(identical(X,Y)){
    stop('Variable X is identical to variable Y. Please check input.')
  }
  if(isTRUE(na.rm)){ #achtung irgendwas stimmt hier nicht
    noNaNInd <- which(is.finite(X)&is.finite(Y))
    X <- X[noNaNInd]
    Y <- Y[noNaNInd]
  }
  ccc=round(cor(X,Y,method = CorMethod),2)
  
  main=paste(main,ccc)
ggobject=DensityScatter(X,Y,DensityEstimation=DensityEstimation,na.rm=FALSE,SampleSize =SampleSize,
             
             NrOfContourLines=NrOfContourLines,Plotter='ggplot', DrawTopView = TRUE,
             
             xlab=xlab, ylab=ylab, main=main,
             
             xlim=xlim, ylim=ylim, Legendlab_ggplot=Legendlab_ggplot,...)$Handle
#print(ggobject)
#ggExtra::ggMarginal(ggobject, type="histogram", bins = 25,X='X',Y='Y')
return(ggobject)  
}