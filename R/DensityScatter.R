DensityScatter=function(X,Y,DensityEstimation="SDH",SampleSize,na.rm=FALSE,PlotIt=TRUE,
                              
                              NrOfContourLines=20,Plotter='native', DrawTopView = TRUE,
                              
                              xlab, ylab, main="DensityScatter",
                              
                              xlim, ylim, Legendlab_ggplot="value",...){
# DensityScatter
#  plot the PDE on top of a scatterplot
#
#  INPUT
#  X[1:n]                  First feature
#  Y[1:n]                  Second feature
#  DensityEstimation        Either "PDE" or "SDH"
#  OPTIONAL
#  SampleSize             Sample Size in case of big data
#  na.rm                   Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE

#  ylab                    Label for the Y axis
#  Legendlab_ggplot               Label for the legend
#  Plotter                 Plotting Backend to use.
#                          Possible values are: native, ggplot, plotly
#
#  OUTPUT
#   X,Y
#  Densities               Numer of points within the Radiuis or SDH of each point
#  ParetoRadius            ParetoRadius used for PDEscatter.
#  Handle                  Handle of the plot object. NULL if native R plot is used.
#
#  Author MT 07/2020
  ##############
  if (!requireNamespace('MBA',quietly = TRUE)){
    
    message('Subordinate package (MBA) is missing. No computations are performed.
Please install the package which is defined in "Suggests".')
    
    return('Subordinate package (MBA) is missing. No computations are performed.
Please install the package which is defined in "Suggests".')
  }
 
  ## Input check
  
  if(missing(xlab)) xlab=deparse1(substitute(X))
  if(missing(ylab)) ylab=deparse1(substitute(Y))
  
  X=checkFeature(X,varname = 'X',Funname = "DensityScatter")
  Y=checkFeature(Y,varname = 'Y',Funname = "DensityScatter")
  if(identical(X,Y)){
    stop('DensityScatter: Variable X is identical to variable Y. Please check input.')
  }

  isnumber=function(X) return(is.numeric(X)&length(X)==1)

  if(missing(SampleSize)){
    SampleSize =-1
  }
  
  if(!isnumber(SampleSize))
    stop('DensityScatter: "SampleSize" is not a numeric number of length 1. Please change Input.')
  
  
  if(!isnumber(NrOfContourLines))
    stop('DensityScatter: "NrOfContourLines" is not a numeric number of length 1. Please change Input.')
  
  if(!is.logical(na.rm))
    stop('DensityScatter: "na.rm" is expected to be either TRUE or FALSE')
  
  if(!is.logical(PlotIt)){
    if(!(PlotIt==-1))
      stop('DensityScatter: "PlotIt" is expected to be either TRUE, FALSE or -1.')
  }
    
  
  if(!is.logical(DrawTopView))
    stop('DensityScatter: "DrawTopView" is expected to be either TRUE or FALSE')

    
  if(isTRUE(na.rm)){ 
    noNaNInd <- which(is.finite(X)&is.finite(Y))
    X = X[noNaNInd]
    Y = Y[noNaNInd]
  }
  
  nData <- length(X)
  if(SampleSize>0){
    if (SampleSize<nData) { 
      sampleInd=sample(1:nData,size = SampleSize)
      X=X[sampleInd]
      Y=Y[sampleInd]
    }
  }
  if(missing(xlim))
    xlim = c(min(X,na.rm = T), max(X,na.rm = T))
  if(missing(ylim))
    ylim = c(min(Y,na.rm = T), max(Y,na.rm = T))


	data <- cbind(X,Y)
	nData <- length(X)


if(DensityEstimation=="PDE"){
  requireNamespace('parallelDist')
  V=PDEscatter(X,Y,SampleSize,na.rm=FALSE,PlotIt=-1,...)
  Densities=V$Densities
}else if(DensityEstimation=="SDH"){
  V=SmoothedDensitiesXY(X=X,Y=Y,PlotIt=FALSE,...)
  Densities=V$Densities
  #X and Y remain the same
}else if(DensityEstimation=="kde2d"){
	#flos density ansatz
  densityMap = MASS::kde2d(data[,1], data[,2], n = 100)
  Densities = sapply(1:nrow(data), function(i){
    densityMap$z[
      which.min(abs(densityMap$x - data[i,1])),
      which.min(abs(densityMap$y - data[i,2]))
      ]
  })
}else{
  stop('DensityScatter: Please choose "DensityEstimation" with eihter "PDE" or "SDH"')
}
	## Plotting now in zplot (again)

	if(DrawTopView){
	  # Assign labels to axis/legend/...
	  switch(Plotter,'ggplot'={
	    plt = zplot(x = X,y = Y,z = Densities,DrawTopView,NrOfContourLines, TwoDplotter = Plotter, xlim = xlim, ylim = ylim)
	    
	    plt <- plt +
	      xlab(xlab) +
	      ylab(ylab) +
	      labs(title=main, fill=Legendlab_ggplot) +
	      theme(panel.background = element_blank())
	    if(isTRUE(PlotIt))
	      print(plt)

	  },'native'={
	    
	    colpalette=colorRampPalette(c("blue","turquoise","green","yellow","orange","red"))
	    noNaNInd <- is.finite(X) & is.finite(Y)
	    colpal <- cut(Densities, length(Densities), labels = FALSE)
	    cols <- rep(NA_character_, length(noNaNInd))
	    #colramp = colorRampPalette(blues9[-(1:3)])
	    cols[noNaNInd] <- colpalette(length(Densities))[colpal]
	    plot(X[noNaNInd],Y[noNaNInd],col=cols,pch=".",xlim = xlim,
	         ylim = ylim,xlab="",ylab="",main="",...)
	    
	    title(main = main, xlab = xlab, ylab = ylab)
	    plt <- 'Native does not have a Handle'
	    if(!isTRUE(PlotIt)) warning('for native plotting cannot be disabled')
	  }, 'plotly'={
	    plt = zplot(x = X,y = Y,z = Densities,DrawTopView,NrOfContourLines, TwoDplotter = Plotter, xlim = xlim, ylim = ylim)
	    
	  requireNamespace('plotly')
	    plt <- plotly::layout(plt,xaxis= list(title=xlab),
	                                  yaxis= list(title=ylab),
	                                  title= main)
	    if(isTRUE(PlotIt))
	      print(plt)

	  })
	}else{
	  switch(Plotter,'ggplot'={
	    plt = zplot(x = X,y = Y,z = Densities,DrawTopView,NrOfContourLines, TwoDplotter = Plotter, xlim = xlim, ylim = ylim)
	    
      print('Plotly plot is used because ggplot is not implemented for option DrawTopView=FALSE.')
	    requireNamespace('plotly')
	    plt <- plotly::layout(plt,scene=list(xaxis= list(title=xlab),
	                                             yaxis= list(title=ylab),zaxis= list(title='PDE'),
	                                             title= main))
	    if(isTRUE(PlotIt))
	      print(plt)
	  },'native'={
	    plt = zplot(x = X,y = Y,z = Densities,DrawTopView,NrOfContourLines, TwoDplotter = Plotter, xlim = xlim, ylim = ylim)
	    
	    print('Plotly plot is used because native is not implemented for option DrawTopView=FALSE.')
	    requireNamespace('plotly')
	    plt <- plotly::layout(plt,scene=list(xaxis= list(title=xlab),
	                                             yaxis= list(title=ylab),zaxis= list(title='PDE'),
	                                             title= main))
	    if(isTRUE(PlotIt))
	      print(plt)
	  }, 'plotly'={
	    plt = zplot(x = X,y = Y,z = Densities,DrawTopView,NrOfContourLines, TwoDplotter = Plotter, xlim = xlim, ylim = ylim)
	    
	    requireNamespace('plotly')
	    plt <- plotly::layout(plt,scene=list(xaxis= list(title=xlab),
	                                  yaxis= list(title=ylab),zaxis= list(title='PDE'),
	                                  title= main))
	    if(isTRUE(PlotIt))
	      print(plt)
	    
	  })
	}
	
	return(invisible(list(X=X,Y=Y,Densities=Densities,Handle=plt)))
}

