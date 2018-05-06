InspectScatterOfData=function(Data,Names=colnames(Data)){
  
  requireNamespace('AdaptGauss')
  panel.histopt <- function(x,...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    x = x[!is.infinite(x)]
    MinD = min(x, na.rm = TRUE)
    MaxD = max(x, na.rm = TRUE)
    optNrOfBins=AdaptGauss::OptimalNoBins(x)
    optNrOfBins = min(100,optNrOfBins) 
    optBreaks <- seq(MinD, MaxD, abs(MinD-MaxD)/optNrOfBins) # bins haben alle die gleiche groesse
    temp <- hist(x, breaks=optBreaks, plot=FALSE)
    Breaks <- temp$breaks
    nB <- length(Breaks)
    y <- temp$counts
    rect(Breaks[-nB], 0, Breaks[-1], y/max(y), col="blue", border="light blue",...)
    # axis(1,col="black",las=1,xaxs='i') #x-Achse
    # axis(2,col="black",las=1,yaxs='i') #y-Achse
    # title(ylab=ylab)
    
  }
  pairs(Data,labels=Names, diag.panel=panel.histopt)
  
}