zplot <- function(x,y,z,DrawTopView = TRUE,NrOfContourLines = 20, TwoDplotter = 'native', xlim, ylim){
#    plotobject = zplot(x,y,z)
#    plotobject = zplot(x,y,z,DrawTopView,NrOfContourLines,TwoDplotter)
#    plots z above xy plane as mountain
#    INPUT
#    x,y,z   the three coordinates to plot may also be given as 3D array XYZ
#    OPTIONAL
#    DrawTopView             ==1 means contur is drawn, otherwise a 3D plot is drawn  (default:DrawTopView==0);
#    NrOfContourLines        number of contour lines to be drawn
#    TwoDplotter               if DrawTopView==1: use the following engine to plot.
#                             legal values: "ggplot", "native", "plotly"
#
#    OUTPUT
#    plotobject             as returned by plotting routines
#    taken from matlabs 2005 version of ALU
#    Implemented in R: FP 3/2016
  isnumber=function(x) return(is.numeric(x)&length(x)==1)
  
  if(!isnumber(NrOfContourLines))
    stop('"NrOfContourLines" is not a numeric number of length 1. Please change Input.')
  

  if(!is.vector(x)){
    x=as.vector(x)
    warning('x is not a vector. Calling as.vector()')
  }
  if(!is.numeric(x)){
    x=as.numeric(x)
    warning('x is not a numeric. Calling as.numeric()')
  }
  if(!is.vector(y)){
    y=as.vector(y)
    warning('y is not a vector. Calling as.vector()')
  }
  if(!is.numeric(y)){
    y=as.numeric(y)
    warning('y is not a numeric. Calling as.numeric()')
  }
  if(!is.vector(z)){
    z=as.vector(z)
    warning('z is not a vector. Calling as.vector()')
  }
  if(!is.numeric(z)){
    z=as.numeric(z)
    warning('z is not a numeric. Calling as.numeric()')
  }
  if(missing(xlim))
    xlim = c(min(x), max(x))
  if(missing(ylim))
    ylim = c(min(y), max(y))

  requireNamespace('plyr')
  if(missing(y) && missing(z)){
    # All Dataparameters given as single matrix.
    data = as.matrix(x)
  } else {
    data = cbind(x,y,z)
  }

  nrofbins = 100 # Magic number taken from matlabcode
  minx = min(data[,1])
  maxx = max(data[,1])
  xbins = seq(minx, maxx, length.out = nrofbins)

  miny = min(data[,2])
  maxy = max(data[,2])
  ybins = seq(miny, maxy, length.out = nrofbins)

  # Jetzt: Interpolation mit akima
#  requireRpackage('akima')

  # Aequivaltent zu Griddata in Matlab. Arbeitet nur leider linear.
  # Ergebnis trotzdem sehr nah an Matlab.
  requireNamespace('akima')
  fld <- akima::interp(x=data[,1],
                y=data[,2],
                z=data[,3],
                xo=xbins,
                yo=ybins,
                linear=T,
                duplicate = 'mean')



  if(!isTRUE(DrawTopView)){

    #### Option 1: Plotly. Plotly ist ein im Backend auf Javascript basierendes Plotingframework
    xaxis = list(title = "X")
    yaxis = list(title = "Y")
    zaxis = list(title = "Z")

#    requireRpackage("plotly")
requireNamespace('plotly')
    # Aus Gruenden erwartet plotly die Matrix transponiert zur R implementation
    return(plotly::plot_ly(x = fld$x, y = fld$y, z = t(fld$z), type="surface", colors = DataVisualizations::PmatrixColormap) %>%
      plotly::layout(scene =list(xaxis = xaxis, yaxis = yaxis, zaxis = zaxis)))
    ####

    #### Option 2: rgl. Kann GAR keine colormaps?
#     requireRpackage("rgl")
#     requireRpackage("evd")
#     persp3d(x = fld$x, y = fld$y, z = fld$z)
  } else {
    switch(TwoDplotter,'ggplot'={
      kernels <- as.matrix(expand.grid(x = xbins, y = ybins))
      naz = which(is.na(fld$z))
      fld$z[naz] = 0
      df = data.frame(x = kernels[,1], y = kernels[,2], z = as.vector(fld$z))

      # Aufbau des Plots
      plt <- ggplot() +
        geom_raster(data = df, aes(x = x, y = y, fill=z))+ # Fuer den hintergrund
        geom_contour(data = df, aes(x = x, y = y, z = z), bins = NrOfContourLines, colour = "grey") +
        scale_fill_gradientn(colors = DataVisualizations::PmatrixColormap) +
        coord_cartesian(xlim = xlim, ylim = ylim)
      #     if(PlotPoints){
      #       df2 <- data.frame(cbind(x,y))
      #       plt <- plt + geom_point(data = df2, aes(x,y), colour = 'blue')
      #     }

      return(plt)

    }, 'native'={
      # R eigener Contourplot
      colormapWrapper <- function(n){
        tmp <- as.numeric(cut(1:length(DataVisualizations::PmatrixColormap),n))
        fun <- function(x){median(which(tmp == x))}
        return(DataVisualizations::PmatrixColormap[as.integer(apply(t(t(unique(tmp))), 1, fun))])
      }

      filled.contour(x = fld$x,
                     y = fld$y,
                     z = fld$z,
                     nlevels = NrOfContourLines,
                     color.palette = colormapWrapper,
                     xlim = xlim,
                     ylim = ylim)
    }, 'plotly'={
      ### Plotly Contourplot. leider schlechter dokumentiert und keine NrOfContourLines
      return(plotly::plot_ly(x = fld$y, y =fld$x, z = t(fld$z), type = "contour", colors = DataVisualizations::PmatrixColormap))
    })
  }
}