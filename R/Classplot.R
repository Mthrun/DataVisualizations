Classplot=function(X, Y,Cls,Names=NULL,na.rm=FALSE, xlab = "X", ylab = 'Y', 
                       main = "Class Plot", Colors,LineColor=NULL,Size=8,LineWidth=1,LineType=NULL,Showgrid=TRUE, PlotIt=TRUE,SaveIt = FALSE){
  
  if(missing(Cls)) Cls=rep(1,length(X))
  
  if(isTRUE(na.rm)){ #achtung irgendwas stimmt hier nicht
    noNaNInd <- which(is.finite(X)&is.finite(Y))
    X = X[noNaNInd]
    Y = Y[noNaNInd]
    Cls=Cls[noNaNInd]
  }
  
  X=checkFeature(X,'X')
  Y=checkFeature(Y,'Y')
  Cls=checkCls(Cls,length(Y))
  if(length(X)!=length(Y)) stop('X and Y have to have the same length')
  
  if(missing(Colors)){
    mc=length(unique(Cls))
    Colors=DataVisualizations::DefaultColorSequence[1:mc]
  }
  p <- plotly::plot_ly(type='scatter',mode='markers',colors=Colors,marker = list(size = Size))
  
  if(!is.null(LineColor)){
    p <- plotly::add_lines(p, x = ~X, y = ~Y,line = list(color = LineColor, width = LineWidth, dash = LineType), name = 'Line')
  }
   
  if(is.null(Names))
    p <- plotly::add_trace(p, x = ~X, y = ~Y,color=~as.factor(Cls), name = ' ')
  else
    p <- plotly::add_trace(p, x = ~X, y = ~Y,color=~as.factor(Names))
  
  p <- plotly::layout(p, title = main, 
                      xaxis = list(title = xlab, showgrid = Showgrid), 
                      yaxis = list(title = ylab, showgrid = Showgrid))
  if(isTRUE(PlotIt))
    p
  
  
  if (isTRUE(SaveIt)) {
    requireNamespace("htmlwidgets")
    htmlwidgets::saveWidget(p, file = "Classplot.html")
  }
  return(p)
}

