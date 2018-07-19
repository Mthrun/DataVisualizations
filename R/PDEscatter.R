PDEscatter=function(x,y,na.rm=FALSE,paretoRadius=0,sampleSize=round(sqrt(500000000),-3),
                              
                              NrOfContourLines=20,Plotter='native', DrawTopView = T,
                              
                              xlab="X", ylab="Y", main="PDEscatter",
                              
                              xlim, ylim, Legendlab_ggplot="value"){
# PDEscatter
#  plot the PDE on top of a scatterplot
#
#  INPUT
#  x[1:n]                  First feature
#  y[1:n]                  Second feature
#  OPTIONAL
#  na.rm                   Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE
#  ParetoRadius            The Pareto Radius; if ==0 or not given it will be calculated by ParetoRadius
#  sampleSize               bottleneck is memmory allocation von matrizen, 500000000 entspricht ca 4gb memory momentan
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
    stop('Variable x is identical to variable y. Please check input.')
  }

  isnumber=function(x) return(is.numeric(x)&length(x)==1)

  if(!isnumber(paretoRadius))
    stop('"paretoRadius" is not a numeric number of length 1. Please change Input.')

  if(!isnumber(sampleSize))
    stop('"sampleSize" is not a numeric number of length 1. Please change Input.')
  
  if(!isnumber(NrOfContourLines))
    stop('"NrOfContourLines" is not a numeric number of length 1. Please change Input.')
  
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
  if(isTRUE(na.rm)){ #achtung irgendwas stimmt hier nicht
    noNaNInd <- which(is.finite(x)&is.finite(y))
    x <- x[noNaNInd]
    y <- y[noNaNInd]
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
	if (sampleSize<nData) { # sample with uniform distribution MaximumNrSamples
	  #warning('More Data than sampleSize. Consider raising sampleSize or using PDEscatterApprox')
	  sampleInd <- floor(nData*c(runif(sampleSize))+1)
	  sampleData = percentdata[sampleInd,]
	} else {
	  sampleData = percentdata
	}
	##########

	# Pspheres mit prozentuierten Daten zu
	# berechnen vermindert Darstellungsfehler


	#Dists = dist(sampleData)
	Dists=parallelDist::parDist(sampleData,method = 'euclidean',diag = F,upper = F)
	Dists=as.vector(Dists)

	if(paretoRadius <= 0){
		 # paretoRadius <- paretoRadiusForGMM(Data = data)
	  #paretoRadius <- prctile(Dists, 6) # aus Matlab uerbernommen
	  # if(sampleSize<nData)
	    paretoRadius <- quantile(Dists, 6/100, type = 5, na.rm = TRUE)

      if(paretoRadius==0){
        paretoRadius <- quantile(Dists, 20/100, type = 5, na.rm = TRUE) #pareto 20/80 rule
        if(paretoRadius==0){
          stop(paste0('Estimation of Radius(',paretoRadius,') for two-dimensional density not possible. Please provide paretoRadius manually.'))
        }else{
        warning(paste0('Estimation of Radius(',paretoRadius,') for two-dimensional density may not work properly. You can provide paretoRadius manually.'))
        }
      }
	  # else
	  #   cquantile(Dists[is.finite(Dists)], 6/100)
	}

	# Ersetzt InPShere2D
	# inPSpheres = as.numeric(colSums(1 * (as.matrix(dist(percentdata)) <= paretoRadius)))

	inPSpheres = inPSphere2D(percentdata, paretoRadius)

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

	  },'native'={
	    title(main = main, xlab = xlab, ylab = ylab)
	    plt <- 'Native does not have a Handle'
	  }, 'plotly'={
	  requireNamespace('plotly')
	    plt <- plt %>% plotly::layout(xaxis= list(title=xlab),
	                                  yaxis= list(title=ylab),
	                                  title= main)

	  })
	}else{
	  switch(Plotter,'ggplot'={
      print('Plotly plot is used because ggplot is not implemented for option DrawTopView=FALSE.')
	    requireNamespace('plotly')
	    plt <- plt %>% plotly::layout(scene=list(xaxis= list(title=xlab),
	                                             yaxis= list(title=ylab),zaxis= list(title='PDE'),
	                                             title= main))
	  },'native'={
	    print('Plotly plot is used because native is not implemented for option DrawTopView=FALSE.')
	    requireNamespace('plotly')
	    plt <- plt %>% plotly::layout(scene=list(xaxis= list(title=xlab),
	                                             yaxis= list(title=ylab),zaxis= list(title='PDE'),
	                                             title= main))
	  }, 'plotly'={
	    requireNamespace('plotly')
	    plt <- plt %>% plotly::layout(scene=list(xaxis= list(title=xlab),
	                                  yaxis= list(title=ylab),zaxis= list(title='PDE'),
	                                  title= main))
	    
	  })
	}
	
	return(invisible(list(AnzInPSpheres=inPSpheres,ParetoRadius=paretoRadius,Handle=plt)))
}

