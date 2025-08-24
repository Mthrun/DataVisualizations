PlotGraph2D <- function(AdjacencyMatrix, Points, Cls, Colors,
                        xlab = "X", ylab = "Y", xlim, ylim,
                        Plotter = "native", LineColor= "grey",
                        pch, lwd,
                        main = "", mainSize,SampleSize=NULL){
  # PlotGraph2D(AdjacencyMatrix,Points,xlab,ylab,LineColor)
  # plots a graph as in graph theory for an AdjacencyMatrix
  # given the 2D coordinates of the points
  #
  # INPUT
  # AdjacencyMatrix[1:n, 1:n]  [1:n,1:n] numerical matrix consting of binary values. 1 indicates that two points have an edge, zero that they do not

  # Points[1:n, 1:d]      [1:n,1:2] numeric matrix of two feature
  #OPTIONAL
  # \item{Cls}{
  #   [1:n] numeric vector of k classes, if not set per default every point is in first class
  # }
  # \item{Colors}{
  #   Optional, string defining the k colors, one per class
  # }
  # \item{xlab}{
  #   Optional, string for xlabel
  # }
  # \item{ylab}{
  #   Optional, string for ylabel
  # }
  # \item{xlim}{
  #   Optional, [1:2] vector of x-axis limits
  # }
  # \item{ylim}{
  #   Optional, [1:2] vector of y-axis limits
  # }
  # \item{Plotter}{
  #   Optional, type of plot: native or plotly
  # }
  # \item{LineColor}{
  #   Optional, color of edges
  # }
  # \item{pch}{
  #   Optional, shape of point, usally can be in a range from zero to 25, see pch of plot for details
  # }
  # \item{lwd}{
  #   width of the lines
  # }
  # \item{main}{
  #   character: title of plot
  # }
  # \item{mainSize}{
  #   numeric: size of title
  # }
  
  # OUTPUT:
  # Plot
  # Author MCT, 04/2023, QS 07/2023
  Plotter=tolower(Plotter)
  if(Plotter=="native"){
    if(missing(pch)){
      pch=20
    }
    if(missing(lwd)){
      lwd=0.1
    }
  }
  if(Plotter=="plotly"){
    if(missing(pch)){
      pch=10
    }
    if(missing(lwd)){
      lwd=0.5
    }
  }
  d = ncol(Points)
  x_vertex = Points[,1] 
  y_vertex = Points[,2] 
  n=length(x_vertex)
  if(dim(AdjacencyMatrix)[1]!=dim(AdjacencyMatrix)[2]){
    warning("PlotGraph2D: AdjacencyMatrix is not nxn")
  }
  if(dim(AdjacencyMatrix)[1]!=n){
    warning("PlotGraph2D: AdjacencyMatrix size does not equal number of points.")
  }

  if(missing(xlim)){
    xlim=c(min(x_vertex),max(x_vertex))
  }
  
  if(missing(ylim)){
    ylim=c(min(y_vertex),max(y_vertex))
  }

  if(missing(Cls))
    Cls=rep(1,n)
  if(missing(mainSize))
    mainSize=1.2
  
  u = unique(Cls)
  uu = sort(u, decreasing = F)
  
  if (missing(Colors)) {
    mc = length(uu)
    if (mc > 1)
      Colors = DataVisualizations::DefaultColorSequence[-2][1:mc] #no yellow
    else
      Colors = "black"
  }
  
  X1 = Y1 = X2 = Y2 = c()
  #too slow
  # for(i in 2:n){
  #   for(j in 1:(i-1)){
  #     if(AdjacencyMatrix[i, j] > 0) { # kante i-> j zeichnen
  #       X1 = c(X1, x_vertex[i])
  #       Y1 = c(Y1, y_vertex[i])
  #       X2 = c(X2, x_vertex[j])
  #       Y2 = c(Y2, y_vertex[j])
  #     }
  #   }
  # }
  List=get_edges(AdjacencyMatrix,x_vertex,y_vertex)
  X1=List$X1
  X2=List$X2
  Y1=List$Y1
  Y2=List$Y2
  
  if(!is.null(SampleSize)){
    ind=sample(1:length(X1),min(SampleSize,length(X1)))
    X1 = X1[ind]
    Y1 = Y1[ind]
    X2 = X2[ind]
    Y2 = Y2[ind]
  }
  if(Plotter == "native"){
    ColorVec = Cls * 0
    k = 1
    for (i in uu) {
      ColorVec[Cls == i] = Colors[k]
      k = k + 1
    }

    plot(x_vertex[1], y_vertex[1], col = 'white', type = 'p',
         xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
         pch = pch,main=main,cex.main=mainSize)  # die knoten
    
    for(i in 1:length(X1)){
      points(c(X1[i], X2[i]), c(Y1[i], Y2[i]), col = LineColor, type = 'l', lwd = lwd)
    }
    #for(i in 2:n){
    #  for(j in 1:(i-1)){
    #    if (AdjacencyMatrix[i, j] > 0) { # kante i-> j zeichnen
    #      FromToX = c(x_vertex[i], x_vertex[j])
    #      FromtoY = c(y_vertex[i], y_vertex[j])
    #      #par(new = TRUE)
    #      points(FromToX, FromtoY, col = LineColor, type = 'l', lwd = lwd)
    #    }# end if verbindung zeichnen
    #  }# end for j
    #}# end for i
    points(x_vertex, y_vertex, col = ColorVec, type = 'p', pch = pch)
  }else{
    if(!requireNamespace("plotly",quietly = TRUE)){
      stop("DelaunayClassificationError: requires library plotly")
    }
    N = dim(Points)[1]
    plotOut = plotly::plot_ly()
    plotOut = plotly::add_segments(plotOut, x = X1, y = Y1, xend = X2, yend = Y2, 
                                   opacity = 1, line = list(color = LineColor, width = lwd),
                                   inherit = F, showlegend=FALSE)
    plotOut = plotly::add_markers(p = plotOut, x = Points[,1], y = Points[,2], type = "scatter",
                                  marker = list(size = pch, color = Colors[Cls]))
    #plotOut = plotly::hide_colorbar(p = plotOut)
    #plotOut = plotly::hide_legend(p = plotOut)
    plotOut = plotly::layout(p = plotOut,
                             title = list(text = paste0(main),font = list(size=mainSize)),
                             xaxis = list(title=xlab, range = xlim),
                             yaxis = list(title=ylab, range = ylim))
    return(plotOut)
  }
} 

