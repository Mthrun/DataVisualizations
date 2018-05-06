InspectVariable=function(Feature,N='Feature',i=1,xlim,ylim,sampleSize=100000,main){
#InspectVariable(Feature,i,N)
# ermoeglichst eine Schnelle Verteilungsbetrachtung einzelner variable
#
# INPUT
# Feature[1:n]               Vector of Data to be plotted
#
# OPTIONAL
# N                       string, welcher Variablennamen angibt
# i                       In for schleife bei vielen Variablen, Nummer der Variablen
# xlim[2]                 x-Achsengrenzen fuer PDEplot
# ylim[2]                 y-Achsengrenzen fuer PDEplot
# sampleSize              default(100000), sample size, if datavector is to big
# OUTPUT
#
# uses PDEplot()
# uses histopt()
#
# MT 11/2014
  isnumber=function(x) return(is.numeric(x)&length(x)==1)
  
  if(!isnumber(i))
    stop('"i" is not a numeric number of length 1. Please change Input.')
  
  if(!isnumber(sampleSize))
    stop('"sampleSize" is not a numeric number of length 1. Please change Input.')
  
  if(!is.vector(Feature)){
    Feature=as.vector(Feature)
    warning('Feature is not a vector. Calling as.vector()')
  }
  if(!is.numeric(Feature)){
    Feature=as.numeric(Feature)
    warning('Feature is not a numeric. Calling as.numeric()')
  }
  
  def.par <-
    par(no.readonly = TRUE) # save default, for resetting...
  
  # Daten bereinigen
  D = Feature[!is.infinite(Feature)]
  
  MinD = min(D, na.rm = TRUE)
  MaxD = max(D, na.rm = TRUE)
  
  
  #m <- layout(matrix(c(1, 1, 3, 3,1,1,3,3,2,2,3,3,2,2,4,4), 4, 4))
  m <-
    graphics::layout(matrix(c(1, 1, 3, 3, 1, 1, 3, 3, 2, 2, 4, 4, 2, 2, 5, 5), 4, 4))
  par(oma = c(0, 0, 1, 0))#c(u,li,o,re) in
  
  # histogramme
  #par(fig=c(0, .51, 0.5, 0.98), new = TRUE)
  #    optNrOfBins = OptimalNoBins(D)
  #    minData = min(D,na.rm = TRUE)
  #    maxData = max(D,na.rm = TRUE)
  #    i = maxData-minData
  #    optBreaks = seq(minData, maxData, i/optNrOfBins) # bins in fixed intervals
  #    hist(D, breaks=optBreaks,xlab=N)
  requireNamespace('AdaptGauss')
  optNrOfBins=AdaptGauss::OptimalNoBins(D)
  optNrOfBins = min(100,optNrOfBins) #

   optBreaks <- seq(MinD, MaxD, abs(MinD-MaxD)/optNrOfBins) # bins haben alle die gleiche groesse
   temp <- hist(D, breaks=optBreaks, plot=FALSE)
 
   #box();
   Breaks <- temp$breaks;  nB <- length(Breaks)
   y <- temp$counts;
   xlab=N 
   ylab='Frequency' 
   plot(x=c(MinD,MaxD), y=c(0, max(temp$counts,na.rm=TRUE)*1.2), type="n", main='',xaxs='i',yaxs='i',axes=FALSE,xlab=xlab, ylab='',xlim=c(MinD,MaxD), ylim=c(0, max(temp$counts,na.rm=TRUE)*1.2))
   par(mgp=c(2.2,0.6,0)) #Abstand: c(Titel, Label, Achse)
   rect(Breaks[-nB], 0, Breaks[-1], y, col="blue", border="light blue",xlab='',ylab=ylab,xlim=c(MinD,MaxD), ylim=c(0, max(temp$counts,na.rm=TRUE)*1.2))
   axis(1,col="black",las=1,xaxs='i') #x-Achse
   axis(2,col="black",las=1,yaxs='i') #y-Achse
   title(ylab=ylab)

  #histopt(D, '', AxisLabs = TRUE, xlab = N)
  
  #lines(x=a$kernels,y=rep(0,length(a$kernels)),col = "black",lwd = 1)
  
  #Fenster fuer PDEplot
  #par(fig=c(0.49, 1, 0.5, 0.98), new = TRUE)
  if (length(D) > sampleSize) {
    ind = sample(1:length(D), size = sampleSize)
    D2 = D[ind]
  } else{
    D2 = D
  }
  
  pdeVal        = ParetoDensityEstimation(D2)
  
  if (missing(xlim) && missing(ylim)) {
    plot(
      pdeVal$kernels,
      pdeVal$paretoDensity,
      type = 'l',
      xaxs = 'i',
      yaxs = 'i',
      xlab = N,
      ylab = 'PDE',
      col = 'blue'
    )
  } else if (missing(ylim)) {
    plot(
      pdeVal$kernels,
      pdeVal$paretoDensity,
      type = 'l',
      xaxs = 'i',
      yaxs = 'i',
      xlab = N,
      ylab = 'PDE',
      xlim = xlim,
      ylim = NULL,
      col = 'blue'
    )
  } else{
    plot(
      pdeVal$kernels,
      pdeVal$paretoDensity,
      type = 'l',
      xaxs = 'i',
      yaxs = 'i',
      xlab = N,
      ylab = 'PDE',
      xlim = xlim,
      ylim = ylim,
      col = 'blue'
    )
  }
  
  #Fenster fuer QQplot
  #par(mgp=c(2,0.5,0)) #Abstand: c(Titel, Label, Achse)
  #plot(x=c(-4.5,4.5),y=c(min(Feature),max(Feature)), xlab="Normalverteilung", ylab=N,axes=TRUE,type='n',xlim=c(-4.5,4.5),ylim=c(min(Feature),max(Feature)))
  #par(mar=c(3,4,2,1)) #c(u,li,o,re)
  par(pty = "s")# Plot immer quadratisch
  
  qqnorm(
    D2,
    pch = 20,
    col = "blue",
    axes = TRUE,
    xlim = c(-4.5, 4.5),
    ylim = c(MinD, MaxD),
    main = '',
    xlab = "Normal Distribution",
    ylab = N
  )
  axis(4, col = "black", las = 3) #y-Achse
  grid(lty = 'dashed', col = 'black')
  mtext(
    'Normal QQ-Plot',
    side = 3,
    line = 0,
    cex = 1,
    col = "black"
  )
  box(lwd = 1, col = 'White') # box + Liniendick
  # Fenster fuer Box-whisker diagramm
  #par(fig=c(.75, 1, 0, 0.5), new = TRUE)
  par(pty = "m")
  boxplot(
    D,
    axes = FALSE,
    ylim = c(MinD, MaxD),
    xlim = c(0.7, 1.4)
  )
  mtext(
    paste0('Range:[', round(MinD, 2), ',', round(MaxD, 2), ']'),
    side = 3,
    line = 0,
    cex = 0.6,
    col = "black"
  )
  
  
  NaNs = (sum(is.infinite(Feature)) + sum(is.na(Feature))) / length(Feature)
  #if(length(NaNs)>0)
  barplot(
    NaNs,
    ylab = 'NaNs in %',
    main = paste0(round(NaNs, 4), ' %'),
    xlim = c(0, 3),
    ylim = c(0, 1)
  )
  if (any(is.nan(Feature), na.rm = TRUE))
    print(
      'NaNs in Feature found. This message is only important, if after rounding the percent of NaN is zero in the bar plot.'
    )
  if (any(is.infinite(Feature), na.rm = TRUE))
    warning('Infinite values in Feature found.')
  
  
  #else
  #print('No NaNs found')
  
  
  def = par(fig = c(0, 0.9, 0.8, 1), new = TRUE)
  if (missing(main))
    mtext(
      paste('VarNr.:', i, N),
      side = 3,
      line = 1,
      cex = 1.5,
      col = "black"
    )
  else
    mtext(
      main,
      side = 3,
      line = 1,
      cex = 1.5,
      col = "black"
    )
  
  par(def.par)
  #box("outer", col="black")
  
}

