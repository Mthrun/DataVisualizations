PDEscatter=function(x,y,SampleSize,na.rm=FALSE,PlotIt=TRUE,ParetoRadius,sampleParetoRadius,
                              
                              NrOfContourLines=20,Plotter='native', DrawTopView = TRUE,
                              
                              xlab="X", ylab="Y", main="PDEscatter",
                              
                              xlim, ylim, Legendlab_ggplot="value"){
# PDEscatter
#  plot the PDE on top of a scatterplot
#
#  INPUT
#  x[1:n]                  First feature
#  y[1:n]                  Second feature
#  OPTIONAL
#  SampleSize             Sample Size in case of big data
#  na.rm                   Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE
#  ParetoRadius            The Pareto Radius; if ==0 or not given it will be calculated by ParetoRadius
#  sampleParetoRadius      bottleneck is memmory allocation von matrizen, 500000000 entspricht ca 4gb memory momentan
#  NrOfContourLines        Number of contour lines to be drawn
#  kernelfactor            Factor to modify the resolution of the grid used to create the plot. Default: 1. Warning: This can increase runtime extremely!
#  xlab                    Label for the x axis
#  ylab                    Label for the y axis
#  Legendlab_ggplot               Label for the legend
#  Plotter                 Plotting Backend to use.
#                          Possible values are: native, ggplot, plotly
#
#  OUTPUT
#  AnzInPSpheres           Numer of points within the ParetoRadius of each point
#  ParetoRadius            ParetoRadius used for PDEscatter.
#  Handle                  Handle of the plot object. NULL if native R plot is used.
#
#  Author in matlab: ALU 2004
#  Rewrite in R with improved logic Felix Pape 01/2016
#  1.Editor: MT 06/18: bugfixes, parallelDist, input checks

#  requireRpackage('reshape2')
  #requireRpackage('akima')
 # requireRpackage('plotly')
  ##############
  
  requireNamespace('parallelDist')
  ## Input check

  x=checkFeature(x,'x')
  y=checkFeature(y,'y')
  if(identical(x,y)){
    stop('PDEscatter: Variable x is identical to variable y. Please check input.')
  }

  isnumber=function(x) return(is.numeric(x)&length(x)==1)

  if(missing(ParetoRadius)){
    ParetoRadius =0 #default as indication that pareto radius should be computed
  }
  
  if(!isnumber(ParetoRadius))
    stop('PDEscatter: "ParetoRadius" is not a numeric number of length 1. Please change Input.')

  
  if(missing(SampleSize)){
    SampleSize =-1
  }
  
  if(missing(sampleParetoRadius)){
    sampleParetoRadius=round(sqrt(500000000),-3)
  }
  
  if(!isnumber(SampleSize))
    stop('PDEscatter: "SampleSize" is not a numeric number of length 1. Please change Input.')
  
  if(!isnumber(sampleParetoRadius))
    stop('PDEscatter: "sampleParetoRadius" is not a numeric number of length 1. Please change Input.')
  
  
  if(!isnumber(NrOfContourLines))
    stop('PDEscatter: "NrOfContourLines" is not a numeric number of length 1. Please change Input.')
  
  
  if(!is.logical(na.rm))
    stop('PDEscatter: "na.rm" is expected to be either TRUE or FALSE')
  
  if(!is.logical(PlotIt)){
    if(!(PlotIt==-1))
      stop('PDEscatter: "PlotIt" is expected to be either TRUE, FALSE or -1.')
  }
    
  
  if(!is.logical(DrawTopView))
    stop('PDEscatter: "DrawTopView" is expected to be either TRUE or FALSE')

  ## Help function(s)
  toRange=function (data, lower, upper) 
  {
    data <- as.matrix(data)
    if (lower == upper) {
      stop("interval width can not be 0!")
    }
    if (lower > upper) {
      temp <- upper
      upper <- lower
      lower <- upper
    }
    range <- upper - lower
    n <- dim(data)[1]
    d <- dim(data)[2]
    if ((n == 1) & (d > 1)) {
      data <- t(data)
      wasRowVector <- 1
    }
    else {
      wasRowVector <- 0
    }
    nRow <- dim(data)[1]
    nCol <- dim(data)[2]
    min <- apply(data, 2, min, na.rm = TRUE)
    min <- matrix(min, nRow, nCol, byrow = TRUE)
    max <- apply(data, 2, max, na.rm = TRUE)
    max <- matrix(max, nRow, nCol, byrow = TRUE)
    range <- max - min
    range[range == 0] <- 1
    scaleData <- (data - min)/range
    scaleData <- lower + scaleData * (upper - lower)
    if (wasRowVector == 1) {
      scaleData = t(scaleData)
    }
    return(scaleData)
  }
  
  ######

    
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

  # if(isTRUE(na.rm)){
  #   tmp=cbind(x,y)
  #   tmp=tmp[complete.cases(tmp),]
  #   x=tmp[,1]
  #   y=tmp[,2]
  # }
  #NAN removal

	data <- cbind(x,y)
	percentdata <- toRange(data,0,100)
	nData <- length(x)

	##########
	# Wenn mehr Daten als gewollt da sind: Sample ziehen.
	##########
	#distanzmatrix is quadratisch minus diagonale

	if (sampleParetoRadius<nData) { # sample with uniform distribution MaximumNrSamples
	  #warning('More Data than SampleSize. Consider raising SampleSize or using PDEscatterApprox')
	  #sampleInd <- floor(nData*c(runif(sampleInd))+1)
	  par_sampleInd=sample(1:nData,size = sampleParetoRadius,replace = FALSE)
	  #mt: cannot sample data for interpolation z plot
	  # otherwise density plot is to stochastic and often incorrect
	  # x=x[sampleInd]
	  # y=y[sampleInd]
	  sampleData4radius = percentdata[par_sampleInd,]
	} else {
	  sampleData4radius = percentdata
	}
	##########

	# Pspheres mit prozentuierten Daten zu
	# berechnen vermindert Darstellungsfehler


	#Dists = dist(sampleData4radius)
	Dists=parallelDist::parDist(sampleData4radius,method = 'euclidean',diag = F,upper = F)
	Dists=as.vector(Dists)
	if(ParetoRadius <= 0){
		 # ParetoRadius <- ParetoRadiusForGMM(Data = data)
	  #ParetoRadius <- prctile(Dists, 6) # aus Matlab uerbernommen
	  # if(SampleSize<nData)
	  if(nData<500){
	    ParetoRadius <- quantile(Dists, 6/100, type = 5, na.rm = TRUE)
	  }else{
	    ParetoRadius <- c_quantile(Dists[is.finite(Dists)], 6/100)
	  }
      if(ParetoRadius==0){         #pareto 20/80 rule
        if(nData<500){
          ParetoRadius <- quantile(Dists, 20/100, type = 5, na.rm = TRUE)
        }else{
          ParetoRadius <- c_quantile(Dists[is.finite(Dists)], 20/100)
        }
        #ParetoRadius <- quantile(Dists, 20/100, type = 5, na.rm = TRUE)
        if(ParetoRadius==0){
          stop(paste0('Estimation of Radius(',ParetoRadius,') for two-dimensional density not possible. Please provide ParetoRadius manually.'))
        }else{
        warning(paste0('Estimation of Radius(',ParetoRadius,') for two-dimensional density may not work properly. You can provide ParetoRadius manually.'))
        }
      }
	  # else
	  #   cquantile(Dists[is.finite(Dists)], 6/100)
	}

	# Ersetzt InPShere2D
	# inPSpheres = as.numeric(colSums(1 * (as.matrix(dist(percentdata)) <= ParetoRadius)))

	inPSpheres = inPSphere2D(percentdata, ParetoRadius)
  
	Matrix3D=cbind(x,y,inPSpheres)
	if(PlotIt==-1)
	  return(list(X=x,Y=y,Densities=inPSpheres,Matrix3D=Matrix3D,ParetoRadius=ParetoRadius,Handle=NULL))
	
	## Plotting now in zplot (again)
	plt = zplot(x = x,y = y,z = inPSpheres,DrawTopView,NrOfContourLines, TwoDplotter = Plotter, xlim = xlim, ylim = ylim)

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
	
	return(invisible(list(X=x,Y=y,Densities=inPSpheres,Matrix3D=Matrix3D,ParetoRadius=ParetoRadius,Handle=plt)))
}

