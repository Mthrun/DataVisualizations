DensityScatter=function(x,y,DensityEstimation="SDH",SampleSize,na.rm=FALSE,PlotIt=TRUE,
                              
                              NrOfContourLines=20,Plotter='native', DrawTopView = TRUE,
                              
                              xlab="X", ylab="Y", main="DensityScatter",
                              
                              xlim, ylim, Legendlab_ggplot="value",...){
# DensityScatter
#  plot the PDE on top of a scatterplot
#
#  INPUT
#  x[1:n]                  First feature
#  y[1:n]                  Second feature
#  DensityEstimation        Either "PDE" or "SDH"
#  OPTIONAL
#  SampleSize             Sample Size in case of big data
#  na.rm                   Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE

#  ylab                    Label for the y axis
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
  
 
  ## Input check

  x=checkFeature(x,'x')
  y=checkFeature(y,'y')
  if(identical(x,y)){
    stop('DensityScatter: Variable x is identical to variable y. Please check input.')
  }

  isnumber=function(x) return(is.numeric(x)&length(x)==1)

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
    noNaNInd <- which(is.finite(x)&is.finite(y))
    x = x[noNaNInd]
    y = y[noNaNInd]
  }
  
  nData <- length(x)
  if(SampleSize>0){
    if (SampleSize<nData) { 
      sampleInd=sample(1:nData,size = SampleSize)
      x=x[sampleInd]
      y=y[sampleInd]
    }
  }
  if(missing(xlim))
    xlim = c(min(x,na.rm = T), max(x,na.rm = T))
  if(missing(ylim))
    ylim = c(min(y,na.rm = T), max(y,na.rm = T))


	data <- cbind(x,y)
	nData <- length(x)


if(DensityEstimation=="PDE"){
  requireNamespace('parallelDist')
  V=PDEscatter(x,y,SampleSize,na.rm=FALSE,PlotIt=-1,...)
  Densities=V$Densities
  x=V$X
  y=V$Y
}else if(DensityEstimation=="SDH"){
  V=SmoothedDensitiesXY(X=x,Y=y,PlotIt=FALSE,...)
  Densities=V$Densities
  #x and y remain the same
}else{
  stop('DensityScatter: Please choose "DensityEstimation" with eihter "PDE" or "SDH"')
}
	## Plotting now in zplot (again)
	plt = zplot(x = x,y = y,z = Densities,DrawTopView,NrOfContourLines, TwoDplotter = Plotter, xlim = xlim, ylim = ylim)

	if(DrawTopView){
	  # Assign labels to axis/legend/...
	  switch(Plotter,'ggplot'={
	    plt <- plt +
	      xlab(xlab) +
	      ylab(ylab) +
	      labs(title=main, fill=Legendlab_ggplot) +
	      theme(panel.background = element_blank())
	    if(isTRUE(PlotIt))
	      print(plt)

	  },'native'={
	    title(main = main, xlab = xlab, ylab = ylab)
	    plt <- 'Native does not have a Handle'
	    if(!isTRUE(PlotIt)) warning('for native plotting cannot be disabled')
	  }, 'plotly'={
	  requireNamespace('plotly')
	    plt <- plotly::layout(plt,xaxis= list(title=xlab),
	                                  yaxis= list(title=ylab),
	                                  title= main)
	    if(isTRUE(PlotIt))
	      print(plt)

	  })
	}else{
	  switch(Plotter,'ggplot'={
      print('Plotly plot is used because ggplot is not implemented for option DrawTopView=FALSE.')
	    requireNamespace('plotly')
	    plt <- plotly::layout(plt,scene=list(xaxis= list(title=xlab),
	                                             yaxis= list(title=ylab),zaxis= list(title='PDE'),
	                                             title= main))
	    if(isTRUE(PlotIt))
	      print(plt)
	  },'native'={
	    print('Plotly plot is used because native is not implemented for option DrawTopView=FALSE.')
	    requireNamespace('plotly')
	    plt <- plotly::layout(plt,scene=list(xaxis= list(title=xlab),
	                                             yaxis= list(title=ylab),zaxis= list(title='PDE'),
	                                             title= main))
	    if(isTRUE(PlotIt))
	      print(plt)
	  }, 'plotly'={
	    requireNamespace('plotly')
	    plt <- plotly::layout(plt,scene=list(xaxis= list(title=xlab),
	                                  yaxis= list(title=ylab),zaxis= list(title='PDE'),
	                                  title= main))
	    if(isTRUE(PlotIt))
	      print(plt)
	    
	  })
	}
	
	return(invisible(list(X=x,Y=y,Densities=Densities,Handle=plt)))
}

