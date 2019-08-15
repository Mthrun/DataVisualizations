ParetoDensityEstimationV2= function(Data,paretoRadius,kernels=NULL,MinAnzKernels=100){
#  V = ParetoDensityEstimation(Data,ParetoRadius,Kernels)
#  V = ParetoDensityEstimation(Data)
#  ParetoDensity=V$paretoDensity
#  Kernels=V$kernels
#  Estimates the Pareto Density for a one dimensional distibution
#  this is the best density estimation to judge Gaussian Mixtures  of the Data see [Ultsch 2003]
# 
#  INPUT
#  Data                    die eindimensional verteilten Daten
#
#  OPTIONAL
#  paretoRadius            der Pareto Radius, wenn nicht angegeben, wird er berechnet
#  kernels                 Data values at which ParetoDensity is measured , use plot(Kernels,ParetoDensity) for display
#                          wird bestimmt, wenn nicht angegeben oder Kernels ==0
#  MinAnzKernels           Minimale Anzahl Kernls, wenn nicht angegeben oder MinAnzKernelss ==0 =>  MinAnzKernels==100	
# 
#  OUTPUT
#  List with
#  kernels                 Data values at which ParetoDensity is measured , use plot(Kernels,ParetoDensity) for display
#  paretoDensity           die mit dem ParatoRadius ermittelte Dichte
#  paretoRadius            der Pareto Radius
# 
#  Author: MT 2019

###############################################
###############################################
  requireNamespace('pracma') #fuer trapz
  if (!is.vector(Data)) {
    Data = as.vector(Data)
    warning('Beware: ParetoDensityEstimation: Data set not univariate !')
  }
  if (!is.numeric(Data)) {
    Data = as.numeric(Data)
    warning('Beware: ParetoDensityEstimation: Data set not numeric !')
  }
  if(length(unique(Data))<3){
    warning('Less than 3 unqiue values for density estimation. Function may not work')
  }
  if(length(Data)!=sum(is.finite(Data))){
    message('Not all values are finite. Please check of infinite or missing values.')
  }
  Data = Data[is.finite(Data)]
  
  if (length(Data) < 10) {
    warning('Less than 10 datapoints given, ParetoRadius potientially cannot be calcualted.')
  }
  
  if (missing(paretoRadius)) {
    paretoRadius = ParetoRadiusV2(Data)
  } else if (is.null(paretoRadius)) {
    paretoRadius = ParetoRadiusV2(Data)
  } else if (is.na(paretoRadius)) {
    paretoRadius = ParetoRadiusV2(Data)
  } else if (paretoRadius == 0 || length(paretoRadius) == 0) {
    paretoRadius = ParetoRadiusV2(Data)
  } else{
    
  }
  minData = min(Data, na.rm = TRUE)
  maxData = max(Data, na.rm = TRUE)
  
  if (length(kernels) == 0 || ( length(kernels)==1 & kernels == 0)) {
    #MT: Korrektur: statt kernels==0 und im Input Kernels=0
    nBins = OptimalNoBinsV2(Data)
    #MT: MinAnzKernels fehlte
    nBins = max(MinAnzKernels , nBins)
    # mindestzahl von Kernels sicherstellen
    if (nBins > 100) {
      if (nBins > 1E4) {
        #MT: Fehlerabdfang bei zu vielen Bins
        nBins=1E4
        warning('Too many bins estimated, try to transform or sample the data')
      } else{
        nBins=nBins * 3 + 1
      }
    }
    breaks = pretty(c(minData,maxData), n = nBins, min.n = 1)
    nB=length(breaks)
    mids = 0.5 * (breaks[-1L] + breaks[-nB])
    kernels = mids
  }
  nKernels = length(kernels)
  #Randapproximierung
  #  diese Daten liegen am unteren Rand
  lowBInd =  (Data < (minData + paretoRadius))
  lowR = as.matrix(2 * minData - Data[lowBInd], ncol = 1) 
  # diese Daten liegen am obere Rand
  upBInd =  (Data > (maxData - paretoRadius))
  upR <- as.matrix(2 * maxData - Data[upBInd], ncol = 1)
  #extend data by mirrowing
  DataPlus = as.matrix(c(Data, lowR, upR), 1)

  paretoDensity <- rep(0, nKernels)
  for (i in 1:nKernels) {
    lb = kernels[i] - paretoRadius
    ub = kernels[i] + paretoRadius
    isInParetoSphere = (DataPlus >= lb) & (DataPlus <= ub)
    paretoDensity[i] = sum(isInParetoSphere)
  }
 
  area <- pracma::trapz(kernels, paretoDensity)
  #adhoc numerical calc (not preferable)
  #idx = 2:length(kernels)
  #area <- (as.double((kernels[idx] - kernels[idx - 1]) %*% (paretoDensity[idx] + paretoDensity[idx - 1]))/2)
  
  #Fall kernel==0 => area==NAN muss abgefangen werden, passiert vermutlich nur bei unique values <2
  if (area < 0.0000000001 || is.na(area)){
    paretoDensity <- rep(0, nKernels)
  }else{
    paretoDensity <- paretoDensity / area
  }
  
  return (list(
    kernels = kernels,
    paretoDensity = paretoDensity,
    paretoRadius = paretoRadius
  ))
  
}

