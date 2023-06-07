DensityContour=function(X,Y,DensityEstimation="SDH",SampleSize,na.rm=FALSE,PlotIt=TRUE,
                              
                              NrOfContourLines=20,Plotter='ggplot', DrawTopView = TRUE,
                              
                              xlab, ylab, main="DensityContour",
                              
                              xlim, ylim, Legendlab_ggplot="value",AddString2lab="",NoBinsOrPareto=NULL,...){
  
  # DensityContour
  #  Use PDE/SDH in density contour plot
  #
  #  INPUT
  #  X[1:n]                  First feature
  #  Y[1:n]                  Second feature
  #  DensityEstimation       Either "PDE" or "SDH"
  #  OPTIONAL
  #  SampleSize              Sample Size in case of big data
  #  na.rm                   Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE
  
  #  ylab                    Label for the Y axis
  #  Legendlab_ggplot        Label for the legend
  #  Plotter                 Plotting Backend to use.
  #                          Possible values are: ggplot, plotly
  #
  #  OUTPUT
  #   X,Y
  #  Densities               Number of points within the Radius or SDH of each point
  #  ParetoRadius            ParetoRadius used for PDEscatter.
  #  Handle                  Handle of the plot object.
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
    
    if(AddString2lab!=""){
      xlab=paste0(xlab,AddString2lab)
      ylab=paste0(ylab,AddString2lab)
    }
  
    isnumber=function(X) return(is.numeric(X)&length(X)==1)
    
    if(!isnumber(NrOfContourLines))
      stop('DensityContour: "NrOfContourLines" is not a numeric number of length 1. Please change Input.')
    
    if(!is.logical(PlotIt)){
      if(!(PlotIt==-1))
        stop('DensityContour: "PlotIt" is expected to be either TRUE, FALSE or -1.')
    }
      
    if(!is.logical(DrawTopView))
      stop('DensityContour: "DrawTopView" is expected to be either TRUE or FALSE')
    
    if(missing(xlim))
      xlim = c(min(X,na.rm = T), max(X,na.rm = T))
    if(missing(ylim))
      ylim = c(min(Y,na.rm = T), max(Y,na.rm = T))
  
  	
  	calcDens = estimateDensity2D(X,Y,DensityEstimation=DensityEstimation, 
  	                              SampleSize=SampleSize, na.rm=na.rm, NoBinsOrPareto=NoBinsOrPareto)
  	X = calcDens[[1]]
  	Y = calcDens[[2]]
  	Densities = calcDens[[3]]
  	## Plotting now in zplot (again)
  
  	if(DrawTopView){
  	  # Assign labels to axis/legend/...
  	  switch(Plotter,'ggplot'={
  	    plt = zplot(x = X,y = Y,z = Densities,DrawTopView, NrOfContourLines, TwoDplotter = Plotter, xlim = xlim, ylim = ylim)
  	    
  	    plt <- plt +
  	      xlab(xlab) +
  	      ylab(ylab) +
  	      labs(title=main, fill=Legendlab_ggplot) +
  	      theme(panel.background = element_blank())
  	    if(isTRUE(PlotIt))
  	      print(plt)
  
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

