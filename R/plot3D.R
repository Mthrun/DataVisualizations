plot3D = function (Data,Cls,...){
#plot3D(Data,Cls)
#A wrapper for Data with systematic clustering colors for a x,y,z plot combined with a classification
#INPUT
#Data   [1:n,1:d] matrix with d=3, if d>3 only the first 3 dimensions are taken
#Cls    [1:n] numeric vector of the classification of data

#\dots further arguments to be processed by \code{rgl::plot3d}
#OUTPUT
#author: MT 2018

if (!is.matrix(Data)) {
  warning('Data is not a matrix. Using as.matrix()')
  Data = as.matrix(Data)
}
nrow = dim(Data)[1]
ncol = dim(Data)[2]
if (ncol < 2) {
  stop('Data has to have at least two columns')
}
NoColors = FALSE

if (missing(Cls)) {
  Cls = rep(1, nrow)
  NoColors = TRUE
} else{
  if (!is.vector(Cls)) {
    Cls = as.vector(Cls)
    warning('Cls should be a vector.Calling as.vector')
  }
  if (!is.numeric(Cls)) {
    Cls = as.numeric(Cls)
    warning('Cls should be a numeric.Calling as.numeric')
  }
  UniqueC = unique(Cls)
  m = length(UniqueC)
  LcolUnique = DataVisualizations::DefaultColorSequence[1:m]
  Lcol = Cls
  
  for (i in 1:m)
    Lcol[Cls == UniqueC[i]] = LcolUnique[i]
}

requireNamespace('rgl')
if (ncol >= 3) {
  x = Data[, 1]
  y = Data[, 2]
  z = Data[, 3]
  if (NoColors)
    rgl::plot3d(x, y, z, ...)
  else
    rgl::plot3d(x, y, z, col = Lcol, ...)
  if (ncol > 3) {
    warning('Only the first three columns are used.')
  }
}
if (ncol == 2) {
  #plot.new()
  x = Data[, 1]
  y = Data[, 2]
  if (NoColors)
    plot(x, y, pch = 19, ...)
  else
    plot(x, y, col = Lcol, pch = 19, ...)
}

}  #END FUNCTION

