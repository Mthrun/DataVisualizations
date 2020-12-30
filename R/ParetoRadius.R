ParetoRadius <- function(Data ,maximumNrSamples = 10000, plotDistancePercentiles = FALSE){
  # MT: in Matlab als ParetoRadiusfuerGMM.m benannt
  # ParetoRadius <- ParetoRadius(Data)
  # function calculates the paretoRadius for passed gauss mixture modell
  
  # INPUT
  # Data[1:n,1:d]				data array, cases in rows, variables in columns
  # OPTIONAL
  # maximumNrSamples			max number for wich the distance calculation can be done, default 1000
  # plotDistancePercentiles	should the plot of percentiles of distances be done? TRUE (default) means yes, FALSE means no
  
  # OUTPUT
  # ParatoRadius				the paret radius
  
  #MT 2019

  ntemp = sum(is.nan(Data))
  if (ntemp > 0) {
    warning('Data has NaN values, Pareto Radius may not be calculated.')
  }
  ntemp2 = sum(is.na(Data))
  if (ntemp2 > ntemp)
    warning('Data has NA values, Pareto Radius may not be calculated.')
  ntemp3 = sum(is.infinite(Data))
  if (ntemp3 > 0)
    warning('Data has infinite valuues, Pareto Radius may not be calculated.')
  
  nData = length(Data)
  
  if (maximumNrSamples >= nData) {
    # no sampling necessary
    sampleData <- Data
  } else{
    #  sample with uniform distribution MaximumNrSamples
    sampleInd <-
      ceiling(runif(maximumNrSamples, min = 0, max = nData)) # floor(nData*c(runif(maximumNrSamples))+1)
    sampleData <- Data[sampleInd]
  }
  
  # calculate distances
  if(  requireNamespace('parallelDist',quietly = TRUE))
	  distvec = as.vector(parallelDist::parallelDist(
		as.matrix(sampleData),
		method = 'euclidean',
		upper = F,
		diag = F
	  ))
  else{
   temp=dist(as.matrix(sampleData),method = 'euclidean')
   distvec = as.vector(temp[lower.tri(temp,diag=FALSE)])
  }
  # selection of ParetoRadius
  #paretoRadius=quantile(distvec,probs = 18/100,na.rm = T,type=8)#minimal unrealized potential (->Ultsch2005)
  
  if (nData < 4000) {
    #use more precise implelemtation
    paretoRadius <- quantile(distvec, 18 / 100, type = 8, na.rm = TRUE)
  } else{
    #use faster implementation
    paretoRadius <- c_quantile(distvec, 18 / 100)
  }
  if (paretoRadius == 0)
  {
    if (nData < 4000) {
      pzt = quantile(
        distvec,
        probs = c(1:100) / 100,
        na.rm = T,
        type = 8
      )
    } else{
      #use faster implementation
      pzt = c_quantile(distvec, probs = c(1:100) / 100)
    }
    paretoRadius <-
      min(pzt[pzt > 0], na.rm = T) # take the smallest nonzero
  }
  
  if (is.nan(paretoRadius))
    stop('Pareto Radius could not be calculated. (NaN value)')
  
  if (is.na(paretoRadius))
    stop('Pareto Radius could not be calculated. (NA value)')
  
  if (!is.finite(paretoRadius))
    stop('Pareto Radius could not be calculated. (infinite value)')
  
  #    plot of distance distribution
  
  if (plotDistancePercentiles) {
    pzt = quantile(
      distvec,
      probs = c(1:100) / 100,
      na.rm = T,
      type = 8
    )
    plot(
      1:100,
      pzt,
      type = 'l',
      col = 'blue',
      main = 'red = ParetoRatius',
      xlab = 'Percentiles',
      ylab = 'Distances'
    )
    lines(
      x = c(pzt[18], pzt[18]),
      y = c(0, paretoRadius),
      col = 'red'
    )
  }
  #MT:
  #ALUs heuristik, in matlab in PDEplot, hier in dieser Funktion, damit martlabs AdaptGauss
  # die selbe Darstellung benutzt
  if (nData > 1024) {
    paretoRadius = paretoRadius * 4 / (nData ^ 0.2)
    
  }
  return(paretoRadius)
}
