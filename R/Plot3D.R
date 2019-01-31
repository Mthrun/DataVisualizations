Plot3D = plot3D = function(Data,Cls,UniqueColors,size=2,na.rm=FALSE,...){
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
  
  if(missing(UniqueColors))
    LcolUnique = DataVisualizations::DefaultColorSequence[1:m]
  else
    LcolUnique=UniqueColors
  
  Lcol = Cls
  
  for (i in 1:m)
    Lcol[Cls == UniqueC[i]] = LcolUnique[i]
}


if (ncol >= 3) {
  requireNamespace('rgl')
  x = Data[, 1]
  y = Data[, 2]
  z = Data[, 3]
  if (NoColors)
    rgl::plot3d(x, y, z, ...)
  else
    rgl::plot3d(x, y, z, col = Lcol,  ...)

  if (ncol > 3) {
    warning('Only the first three columns are used.')
  }
}
if (ncol == 2) {
  # ggplot2
  #plot.new()
  x = Data[, 1]
  y = Data[, 2]
  DF=data.frame(x=x,y=y)
  p <- ggplot(DF, aes(x, y))
  if (NoColors){
    p <-  p + geom_point(color='blue',size=size,na.rm=na.rm,...)
    # p <-  p +  geom_text_repel(aes(label=colnames(Forecasts)), size=3)
    # plot(x, y, pch = 19, ...)
  }else{
    p <-  p + geom_point(color=Lcol,size=size,na.rm=na.rm,...)
    # p <-  p +  geom_text_repel(aes(label=colnames(Forecasts)), size=3)
    # plot(x, y, col = Lcol, pch = 19, ...)
  }
  p <-p+coord_fixed(ratio = 1)+theme_bw()
  return(p)
}

}  #END FUNCTION

