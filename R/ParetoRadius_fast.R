ParetoRadius_fast <- function(Data ,maximumNrSamples = 10000,na.rm=TRUE,epsilon=1e-9,failsave=FALSE){
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
  if(isTRUE(na.rm)){
    Data=Data[is.finite(Data)]
  }else{
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
  }
  nData = length(Data)
  
  if (maximumNrSamples >= nData) {
    # no sampling necessary
    sampleData <- Data
  } else{
    #  sample with uniform distribution MaximumNrSamples
    # if(isFALSE(failsave)){
      sampleInd <-
        ceiling(runif(maximumNrSamples, min = 0, max = nData)) # floor(nData*c(runif(maximumNrSamples))+1)
      sampleData <- Data[sampleInd]
    # }else{
    #   warning("ParetoRadius: failsave activated to measure density, computing pareto radius on full data without taking a sample.")
    # }
  }
  
  # calculate distances

  paretoRadius=quantileDist1d(sampleData)
  
  if (paretoRadius < epsilon){
    probs=seq(from=18,to=100,by=1)/100
    pzt = quantile4LargeVectors(dist1d(sampleData), probs = probs)

    paretoRadius <-
      min(pzt[pzt > 0], na.rm = T) # take the smallest nonzero
    #    ind=head(which(pzt>0),1)
    # paretoRadius=mean(pzt[ind:(ind+1)])
  }
  
  if (is.nan(paretoRadius)){
    warning('Pareto Radius could not be calculated. (NaN value), fallback to densityEstimation4smallNoCases()')
    return(NaN)
  }

  
  if (is.na(paretoRadius)){
    warning('Pareto Radius could not be calculated. (NA value), fallback to densityEstimation4smallNoCases(')
    return(NaN)
  }

  
  if (!is.finite(paretoRadius)){
    warning('Pareto Radius could not be calculated. (infinite value), fallback to densityEstimation4smallNoCases(')
    return(NaN)
  }

  #MT:
  #ALUs heuristik, in matlab in PDEplot, hier in dieser Funktion, damit martlabs AdaptGauss
  # die selbe Darstellung benutzt
  if (nData > 1024 &isFALSE(failsave)) {
    paretoRadius = paretoRadius * 4 / (nData ^ 0.2)
  }
  return(as.numeric(unlist(paretoRadius)))
}
