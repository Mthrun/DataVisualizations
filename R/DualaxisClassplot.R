DualaxisClassplot=function(X, Y1, Y2,Cls1,Cls2, xlab = "X", y1lab = 'Y1', y2lab = 'Y2', 
main = "Dual Axis Class Plot", Colors,Showgrid=TRUE, SaveIt = FALSE){
 
  Y1=checkFeature(Y1,varname='Y1',Funname="DualaxisClassplot")
  n1=length(Y1)
  if(length(X)!=n1) stop('X and Y have to have the same length')
  
  Cls1=checkCls(Cls1,n1)
  Y2=checkFeature(Y2,varname='Y2',Funname="DualaxisClassplot")
  n2=length(Y2)

  
  if(missing(Cls2)){
    Cls2=Cls1
  }else{
    Cls2=checkCls(Cls2,n2)
  }

  
  if(sum(sort(unique(Cls1))-sort((unique(Cls2))))<2){
    Cls2=Cls2+max(unique(Cls1))
    
  }
  
  if(missing(Colors)){
    mc=length(unique(Cls1))+length(unique(Cls2))
    Colors=DataVisualizations::DefaultColorSequence[1:mc]
  }
  p <- plotly::plot_ly(type='scatter',mode='markers',colors=Colors)
  p <- plotly::add_trace(p, x = ~X, y = ~Y1,color=~as.factor(Cls1), name = y1lab)
  p <- plotly::add_trace(p, x = ~X, y = ~Y2,color=~as.factor(Cls2), name = y2lab, 
                         yaxis = "y2")
  p <- plotly::layout(p, title = main, yaxis2 = list(overlaying = "y", 
                                                     side = "right", title = y2lab, showgrid = Showgrid), xaxis = list(title = xlab, 
                                                                                                                    showgrid = Showgrid), yaxis = list(title = y1lab, showgrid = Showgrid))
  p
  
  
  if (SaveIt) {
    requireNamespace("htmlwidgets")
    htmlwidgets::saveWidget(p, file = "DualClassplot.html")
  }
  return(p)
}

