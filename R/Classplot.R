Classplot=function(X, Y,Cls, xlab = "X", ylab = 'Y', 
                       main = "Class Plot", Colors,LineColor='grey',LineWidth=1,LineType=NULL,Showgrid=TRUE, SaveIt = FALSE){

  Y=checkFeature(Y,'Y')
  Cls=checkCls(Cls,length(Y))
  if(length(X)!=length(Y)) stop('X and Y have to have the same length')
  
  if(missing(Colors)){
    mc=length(unique(Cls))
    Colors=DataVisualizations::DefaultColorSequence[1:mc]
  }
  p <- plotly::plot_ly(type='scatter',mode='markers',colors=Colors)
  p <- plotly::add_lines(p, x = ~X, y = ~Y,line = list(color = LineColor, width = LineWidth, dash = LineType), name = '')
  p <- plotly::add_trace(p, x = ~X, y = ~Y,color=~as.factor(Cls), name = ylab)
  p <- plotly::layout(p, title = main, 
                      xaxis = list(title = xlab, showgrid = Showgrid), 
                      yaxis = list(title = ylab, showgrid = Showgrid))
  p
  
  
  if (SaveIt) {
    requireNamespace("htmlwidgets")
    htmlwidgets::saveWidget(p, file = "Classplot.html")
  }
  return(p)
}

