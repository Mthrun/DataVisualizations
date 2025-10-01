ParetoRadius <- function(Data ,maximumNrSamples = 10000, plotDistancePercentiles = FALSE,Compute="Cpp"){
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
  distvec = dist1d(sampleData)
  
  # selection of ParetoRadius
  #paretoRadius=quantile(distvec,probs = 18/100,na.rm = T,type=8)#minimal unrealized potential (->Ultsch2005)
  Compute=tolower(Compute)
  paretoRadius <- quantile4LargeVectors(distvec, 18 / 100)

  if (paretoRadius == 0){
      pzt = quantile4LargeVectors(distvec, probs = c(1:100) / 100)
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
    #prior solution
    distvec2 = as.vector(parallelDist::parallelDist(
      as.matrix(sampleData),
      method = 'euclidean',
      upper = F,
      diag = F
    ))
    pzt = quantile(
      distvec2,
      probs = c(1:100) / 100,
      na.rm = T,
      type = 8
    )
    #curent solution
    pzt2 = quantile4LargeVectors(
      distvec,
      probs = c(1:100) / 100
    )
    plot(
      1:100,
      pzt,
      type = 'l',
      col = 'orange',
      main = 'purple = ParetoRatius, orange= R quantile, steelblue = Cpp quantile',
      xlab = 'Percentiles',
      ylab = 'Distances',lwd=2
    )
    points(
      1:100,
      pzt2,
      type = 'l',
      col = 'steelblue',lty="dashed",lwd=2
    )
    lines(
      x = c(18, 18),
      y = c(0, pzt[18]),
      col = 'purple',lwd=3
    )
    lines(
      x = c(0, 18),
      y = c(pzt[18],pzt[18]),
      col = 'purple'
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
