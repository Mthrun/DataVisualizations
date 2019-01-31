Silhouetteplot=SilhouettePlot = function(DataOrDistances, Cls=NULL,method= "euclidean",PlotIt=TRUE){
#sil=SilhouettePlot(DataOrDistances, Cls=NULL,method= "euclidean",PlotIt=TRUE) 
#   Silhouette plot for classified data
#   
#   SilhouettePlot plots cluster silhouettes for the n-by-d data
#   matrix Data or distance matrix where the clusters are defined in the vector Cls.
# INPUT
# DataOrDistances   [1:n,1:d] data cases in rows, variables in columns, if not symmetric
#     
#                     or
#     
#                   [1:n,1:n] distance matrix, if symmetric
#   
# Cls          numeric vector, [1:n,1]  classified data  
# method         one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". Any unambiguous substring can be given, see \code{dist}
# PlotIt      FALSE to supress the plot. Default:TRUE

# Output
#   
# silh[1:n]  Silhouette values in a  vector
# 
#
#
# Author Hansen-Goos 2015, major redone by Michael Thrun 2018
  #1.Editor: MT 06/18
  requireNamespace('parallelDist')
  if(!is.matrix(DataOrDistances)){
    warning('DataOrDistances is not a matrix. Calling as.matrix()')
    DataOrDistances=as.matrix(DataOrDistances)
  }
  if(!mode(DataOrDistances)=='numeric'){
    warning('Data is not a numeric matrix. Calling mode(DataOrDistances)="numeric"')
    mode(DataOrDistances)='numeric'
  }
  
  if (is.null(DataOrDistances) || is.null(Cls)) {
    warning('notEnoughInputs')
    
  }
  
  if (isSymmetric(DataOrDistances)) {
    InputDistances = DataOrDistances
  }
  else{
    warning('Distances are not in a symmetric matrix, Datamatrix is assumed and dist() ist called')
    
    #InputDistances = as.matrix(dist(DataOrDistances, method = method, diag =TRUE))
    InputDistances=as.matrix(parallelDist::parDist(DataOrDistances,method = method))
    #DataDists = DistanceMatrix(DataOrDistances, method = method)
  }
  
  nIn = length(Cls)
  if (nIn != dim(InputDistances)[1]) {
    warning('stats:silhouette:InputSizeMismatch')
  }
  
  #  sorts a numeric grouping variable in ascending order (ersetzt [idx,cnames] = grp2idx(Cls); in MAtlab)
  idx = Cls
  temp = table(Cls)
  cnames = names(temp)
  k = length(cnames)
  
  for (i in 1:k) {
    idx[idx == as.numeric(cnames[i])] = i
  }
  
  # get size of the data
  n = length(idx)
  
  p = dim(InputDistances)[2]
  
  # Get number of members of every Group
  count = rep(0, k)
  for (i in 1:k) {
    count[i] = sum(idx == i)
  }
  
  # Create a list of members for each Cluster
  mbrs = matrix(0, n, k)
  mbrs[t(kronecker(matrix(1, 1, n), 1:k)) ==   kronecker(matrix(1, 1, k), idx)] = 1
  
  # Get avg distance from every point to all (other) points in each cluster
  avgDWithin = matrix(Inf, n)
  avgDBetween = matrix(Inf, n, k)
  distjAll = InputDistances#DistanceMatrix(X,method=distance)
  for (j in 1:n) {
    distj = distjAll[j, ]
    
    # Compute average distance by cluster number
    for (i in 1:k) {
      if (i == idx[j]) {
        avgDWithin[j] = sum(distj[mbrs[, i] == 1]) / max(count[i] - 1, 1)
      } else {
        avgDBetween[j, i] = sum(distj[mbrs[, i] == 1]) / count[i]
      }
    }
  }
  
  #Calculate the silhouette values
  minavgDBetween = apply(avgDBetween, 1, min)
  silh = (minavgDBetween - avgDWithin) / apply(cbind(avgDWithin, minavgDBetween), 1, max)
  
  # Create the bars:  group silhouette values into clusters, sort values
  # within each cluster.  Concatenate all the bars together, separated by
  # empty bars.  Locate each tick midway through each group of bars
  space = max(floor(.02 * n), 2)
  bars = matrix(Inf, space)
  
  tcks = 0
  for (i in 1:k) {
    bars = c(bars,-sort(na.last = T, -silh[idx == i]), matrix(Inf, space))
    
    tcks[i] = length(bars)
    
    end
  }
  tcks = tcks - 0.5 * (diff(c(space, tcks)) + space - 1)
  
  # Plot the bars, don't clutter the plot if there are lots of
  # clusters or bars (>20)
  if (k > 20) {
    cnames = ''
  }
  
  if (isTRUE(PlotIt)){
    barplot(
      rev(bars),
      width = 1,
      space = 0,
      ylim = c(1, length(bars)),
      xlim = c(min(c(bars, 0)), 1.1),
      xlab = "Silhouette Value",
      ylab = "Cluster",
      col = "blue4",
      border = NA,
      horiz = TRUE
    )
    axis(2 , at = length(bars) - tcks, labels = cnames)
    box()
  }
  
  return(invisible(silh))
  
}
  
  
  
  
  
  
  
  
  
  
  
