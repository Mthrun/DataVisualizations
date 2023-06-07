estimateDensity2D = function(X,Y,DensityEstimation="SDH", SampleSize, na.rm=FALSE, NoBinsOrPareto=NULL) {
  # estimateDensity2D
  #  Estimates densities for two-dimensional data with the given estimation type
  #
  #  INPUT
  #  X                      [1:n] numerical vector of first feature
  #  Y                      [1:n] numerical vector of second feature
  #  DensityEstimation       Either "PDE","SDH" or "kde2d"
  #  OPTIONAL
  #  SampleSize              Sample Size in case of big data
  #  na.rm                   Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE
  #  NoBinsOrPareto          Density specifc parameters, for PDEscatter(ParetoRadius) or SDH (nbins)) or kde2d(bins)
  #
  #  OUTPUT
  #   X                         [1:m] numerical vector of first feature, m<=n depending if all values are finite an na.rm parameter
  #   Y                           [1:m] numerical vector of second feature, m<=n depending if all values are finite an na.rm parameter
  #  Densities               the density of each two-dimensional data point
  #
  #  Author MCT 07/2020
  # 1. Editor Luca Brinkman and MCT, 06/23
  ##############
  
  if (!requireNamespace('ScatterDensity',quietly = TRUE)) {
    message(
      'Subordinate clustering package (ScatterDensity) is missing. Switching to MASS::kde2d density estimation.
            Please install the package which is defined in "Suggests".'
    )
    DensityEstimation="kde2d"
  } 
  if (!requireNamespace('MASS',quietly = TRUE)) {
    message(
      'Subordinate clustering package (MASS) is missing. Switching to ScatterDensity density estimation.
            Please install the package which is defined in "Suggests".'
    )
    DensityEstimation="SDH"
  }
  if (!requireNamespace('parallelDist',quietly = TRUE)) {
    if(DensityEstimation=="PDE"){
      message(
        'Subordinate clustering package (parallelDist) is missing. Switching to ScatterDensity density estimation.
              Please install the package which is defined in "Suggests".'
      )
      DensityEstimation="SDH"
    }
  }
  Funname = "estimateDensity2D"
  X=checkFeature(X,varname = 'X',Funname = Funname)
  Y=checkFeature(Y,varname = 'Y',Funname = Funname)
  if(identical(X,Y)){
  
    stop(paste0(Funname, ': Variable X is identical to variable Y. Please check input.'))
  }
  
  isnumber=function(X) return(is.numeric(X)&length(X)==1)
  
  if(missing(SampleSize)){
    SampleSize =-1
  }
  
  if(!isnumber(SampleSize))
    stop(paste0(Funname, ': SampleSize is not a numeric number of length 1. Please change Input.'))
  
  
  if(!is.logical(na.rm))
    stop(paste0(Funname, ': "na.rm" is expected to be either TRUE or FALSE'))
  
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
  
  data <- cbind(X,Y)

  if(DensityEstimation=="PDE"){
    #requireNamespace('parallelDist')
    V=ScatterDensity::PDEscatter(X,Y,SampleSize,na.rm=FALSE,PlotIt=-1,ParetoRadius = NoBinsOrPareto)
    Densities=V$Densities
  }else if(DensityEstimation=="SDH"){
    V=ScatterDensity::SmoothedDensitiesXY(X=X,Y=Y,PlotIt=FALSE,nbins = NoBinsOrPareto)
    Densities=V$Densities
    #X and Y remain the same
  }else if(DensityEstimation=="kde2d"){
    #flos density ansatz
    if(!is.null(NoBinsOrPareto))
      densityMap = MASS::kde2d(data[,1], data[,2], n = NoBinsOrPareto)
    else
      densityMap = MASS::kde2d(data[,1], data[,2], n = 100)
    
    Densities = sapply(1:nrow(data), function(i){
      densityMap$z[
        which.min(abs(densityMap$x - data[i,1])),
        which.min(abs(densityMap$y - data[i,2]))
      ]
    })
  }else{
    stop('DensityScatter: Please choose "DensityEstimation" with eihter "PDE", "SDH" or kde2d.')
  }
  
  return(list(X, Y, Densities))
}