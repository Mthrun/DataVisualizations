PlotGraph2D <- function(AdjacencyMatrix,Points,Cls,Colors,xlab,ylab,xlim,ylim,LineColor= 'grey',pch=20,lwd=0.1){
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
  # \item{LineColor}{
  #   Optional, color of edges
  # }
  # \item{pch}{
  #   Optional, shape of point, usally can be in a range from zero to 25, see pch of plot for details
  # }
  # \item{lwd}{
  #   width of the lines
  # }
  
  # OUTPUT:
  # native plote
  #Author MCT, 04/2023
  
  if(missing(xlab)) xlab=paste0(deparse1(substitute(Points)),"[,1]")
  if(missing(ylab)) ylab=paste0(deparse1(substitute(Points)),"[,2]")
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

  if(missing(xlim))
    xlim=c(min(x_vertex),max(x_vertex))
  
  if(missing(ylim))
    ylim=c(min(y_vertex),max(y_vertex))#

  if(missing(Cls))
    Cls=rep(1,n)
  
  u = unique(Cls)
  uu = sort(u, decreasing = F)
  
  if (missing(Colors)) {
    mc = length(uu)
    if (mc > 1)
      Colors = DataVisualizations::DefaultColorSequence[-2][1:mc] #no yellow
    else
      Colors = "black"
  }
  
  ColorVec = Cls * 0
  k = 1
  for (i in uu) {
    ColorVec[Cls == i] = Colors[k]
    k = k + 1
  }
  
  plot(
    x_vertex[1],
    y_vertex[1],
    col = 'white',
    type = 'p',
    xlim = xlim,
    ylim = ylim,
    xlab = xlab,
    ylab = ylab,
    pch = pch
  )  # die knoten
  for (i in 1:n) {
    for (j in 1:n) {
      if (i > j) {
        #AdjacencyMatrix symmetrisch, diagnole irrelevant
        if (AdjacencyMatrix[i, j] > 0) {
          # kante i-> j zeichnen
          FromToX = c(x_vertex[i], x_vertex[j])
          FromtoY = c(y_vertex[i], y_vertex[j])
          #par(new = TRUE)
          points(
            FromToX,
            FromtoY,
            col = LineColor,
            type = 'l',
            lwd = lwd
          )
        }# end if verbindung zeichnen
      }
    }# end for j
  }# end for i
  points(x_vertex,
         y_vertex,
         col = ColorVec,
         type = 'p',
         pch = pch)
  
} 

