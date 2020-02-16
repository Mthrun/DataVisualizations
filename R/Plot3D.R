Plot3D = plot3D = function(Data,Cls,UniqueColors,size=2,na.rm=FALSE,Plotter3D="rgl",...){
#plot3D(Data,Cls)
#A wrapper for Data with systematic clustering colors for a x,y,z plot combined with a classification
#INPUT
#Data   [1:n,1:d] matrix with d=3, if d>3 only the first 3 dimensions are taken
#Cls    [1:n] numeric vector of the classification of data

#\dots further arguments to be processed by \code{rgl::plot3d}
#OUTPUT
#author: MT 2018, edited 2020

if (!is.matrix(Data)) {
  warning('Data is not a matrix. Using as.matrix()')
  Data = as.matrix(Data)
}
nrows = dim(Data)[1]
ncols = dim(Data)[2]
if (ncols < 2) {
  stop('Data has to have at least two columns')
}
NoColors = FALSE

if (missing(Cls)) {
  Cls = rep(1, nrows)
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
  if(length(Cls)!=nrows){
    warning(paste0('Length of Cls (',length(Cls),') does not match number of rows (',nrows,'). Cls is replicated until it matches the number of rows.'))
    Cls=rep(Cls,1000)[1:nrows]
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


if (ncols >= 3) {
  x = Data[, 1]
  y = Data[, 2]
  z = Data[, 3]
  switch(Plotter3D,
         "rgl"={
           rglgiven=requireNamespace('rgl')
              if (NoColors){
                if(isTRUE(rglgiven)){
                  rgl::plot3d(x, y, z,size = size, ...)
                }else{
                  p=plotly::plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers",size = size,...)
                 return(p)
                }
              }else{
                if(isTRUE(rglgiven)){
                  rgl::plot3d(x, y, z, col = Lcol,size = size,  ...)
                }else{
                  p=plotly::plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers", color=as.factor(Cls),colors=unique(Lcol,fromLast = F),size = size,...)
                  return(p)
                }
              }
  },"plotly"={
    plotlygiven=requireNamespace('plotly')
    if (NoColors){
      if(isFALSE(plotlygiven)){
        rgl::plot3d(x, y, z,size = size, ...)
      }else{
        p=plotly::plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers",size = size,...)
        return(p)
      }
    }else{
      if(isFALSE(plotlygiven)){
        rgl::plot3d(x, y, z, col = Lcol,size = size,  ...)
      }else{
        p=plotly::plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers", color=as.factor(Cls),colors=unique(Lcol,fromLast = F),size = size,...)
        return(p)
      }
    }
  })
  if (ncols > 3) {
    warning('Only the first three columns are used.')
  }
}
if (ncols == 2) {
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

