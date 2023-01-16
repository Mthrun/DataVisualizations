ParetoDensityEstimation = function(Data,paretoRadius,kernels=NULL,MinAnzKernels=100,PlotIt=FALSE,Silent=FALSE){
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
 xlab=deparse1(substitute(Data))
  if (!is.vector(Data)) {
    Data = as.vector(Data)
    if(isFALSE(Silent))
    warning('Beware: ParetoDensityEstimation: Data set not univariate! Please provide a vector as Data.')
  }
  if (!is.numeric(Data)) {
    Data = as.numeric(Data)
    if(isFALSE(Silent))
    warning('Beware: ParetoDensityEstimation: Data set not numeric !')
  }
  
  if (length(Data) != sum(is.finite(Data))) {
    message('Not all values are finite. Please check of infinite or missing values.')
  }
  Data = Data[is.finite(Data)]
  values = unique(Data)
  
  if (length(values) > 2 & length(values) < 5) {
    if(isFALSE(Silent))
    warning('Less than 5 unqiue values for density estimation. Function may not work')
  }
  #FLAG_kernels_manualSet=TRUE
  if (length(values) < 3) {
    if(isFALSE(Silent))
    warning(
      '1 or 2 unique values for density estimation. Dirac Delta distribution(s) is(are) assumed. Input of "kernels", "paretoRadius" and "MinAnzKernels" or ignored!'
    )
    
    if (values[1] != 0)
      kernels = seq(from = values[1] * 0.9,
                    to = values[1] * 1.1,
                    by = values[1] * 0.0001)
    else
      kernels = seq(from = values[1] - 0.1,
                    to = values[1] + 0.1,
                    by = 0.0001)
    
    paretoDensity = rep(0, length(kernels))
    paretoDensity[kernels == values[1]] = 1
    
    if (length(values) == 2) {
      if (values[2] != 0)
        kernels2 = seq(from = values[2] * 0.9,
                       to = values[2] * 1.1,
                       by = values[2] * 0.0001)
      else
        kernels2 = seq(from = values[2] - 0.1,
                       to = values[2] + 0.1,
                       by = 0.0001)
      
      
      paretoDensity2 = rep(0, length(kernels2))
      paretoDensity2[kernels2 == values[2]] = 1
      
      paretoDensity = c(paretoDensity, paretoDensity2)
      kernels = c(kernels, kernels2)
    }
    if (isTRUE(PlotIt)) {
      plot(
        kernels,
        paretoDensity,
        type = 'l',
        main = 'RAW PDE rplot',
        xaxs = 'i',
        yaxs = 'i',
        xlab = 'Data',
        ylab = 'PDE',
        ylim=c(0,max(paretoDensity)*1.1)
      )
    }
    return (list(
      kernels = kernels,
      paretoDensity = paretoDensity,
      paretoRadius = 0
    ))
  }# end if if (length(values) < 3)
  
  if (length(Data) < 10) {
    if(isFALSE(Silent))
    warning('Less than 10 datapoints given, ParetoRadius potientially cannot be calcualted.')
  }
  
  if(length(Data)<10e4){#smaller data
    if (missing(paretoRadius)) {#10% or bigger sample is taken
      paretoRadius = ParetoRadius(Data)
    } else if (is.null(paretoRadius)) {
      paretoRadius = ParetoRadius(Data)
    } else if (is.na(paretoRadius)) {
      paretoRadius = ParetoRadius(Data)
    } else if (paretoRadius == 0 || length(paretoRadius) == 0) {
      paretoRadius = ParetoRadius(Data)
    } else{
      #ToNothing because radius is given by user
    }
  }else{#big data
    if (missing(paretoRadius)) {#multiple small samples are taken
      paretoRadius = mean(sapply(1:100, function(x) return(DataVisualizations::ParetoRadius(Data,maximumNrSamples = 1000))),na.rm=TRUE)
    } else if (is.null(paretoRadius)) {
      paretoRadius = mean(sapply(1:100, function(x) return(DataVisualizations::ParetoRadius(Data,maximumNrSamples = 1000))),na.rm=TRUE)
    } else if (is.na(paretoRadius)) {
      paretoRadius = ParetoRadius(Data)
    } else if (paretoRadius == 0 || length(paretoRadius) == 0) {
      paretoRadius = mean(sapply(1:100, function(x) return(DataVisualizations::ParetoRadius(Data,maximumNrSamples = 1000))),na.rm=TRUE)
    } else{
      #ToNothing because radius is given by user
    }
  }
  minData = min(Data, na.rm = TRUE)
  maxData = max(Data, na.rm = TRUE)
  #Update 2022, Mai ----
  #kernels werden im schritt eins immer manuel berechnet egal was der user vorgibt
  #if (length(kernels) <= 1) { #kernels wurden vom user nicht gesetzt
  #  if (length(kernels) == 0 || (length(kernels) == 1 & kernels == 0)) {
      #MT: Korrektur: statt kernels==0 und im Input Kernels=0
      nBins = OptimalNoBins(Data)
      #MT: MinAnzKernels fehlte
      nBins = max(MinAnzKernels , nBins)
      # mindestzahl von Kernels sicherstellen
      if (nBins > 100) {
        if (nBins > 1E4) {
          #MT: Fehlerabdfang bei zu vielen Bins
          nBins = 1E4
          if(isFALSE(Silent))
          warning('Too many bins estimated, try to transform or sample the data')
        } else{
          nBins = nBins * 3 + 1
        }
      }
      breaks = pretty(c(minData, maxData), n = nBins, min.n = 1)
      nB = length(breaks)
      mids = 0.5 * (breaks[-1L] + breaks[-nB])
      kernels_internal = mids
      #FLAG_kernels_manualSet=FALSE
  #  }
  #}
  #bugfix: MT 2020
  #sicherstellen das alle daten auch in einer ParetoKugel enthalten sind
  #if(isFALSE(FLAG_kernels_manualSet)){
    if((kernels_internal[1]-paretoRadius)!=minData){
      kernels_internal=c(minData,kernels_internal)
    } 
    if((tail(kernels_internal,1)+paretoRadius)!=maxData){
      kernels_internal=c(kernels_internal,maxData)
    }
  # }else{#design choice: user entscheidung geht vor
  #   if((kernels_internal[1]-paretoRadius)!=minData){#aber ueble warneldung vorgeben!
  #     if(isFALSE(Silent))
  #     message("ParetoDensityEstimation(): kernels do not contain all datapoints. Density estimation is incomplete. Please either set kernels correctly or let the function set the kernels automatically!")
  #   } 
  #   if((tail(kernels_internal,1)+paretoRadius)!=maxData){
  #     if(isFALSE(Silent))
  #       message("ParetoDensityEstimation(): Kernels do not contain all datapoints. Density estimation is incomplete Please either set kernels correctly or let the function set the kernels automatically!")
  #   }
  # }
  nKernels = length(kernels_internal)
  #Randapproximierung
  #  diese Daten liegen am unteren Rand
  lowBInd =  (Data < (minData + paretoRadius))
  lowR = as.matrix(2 * minData - Data[lowBInd], ncol = 1)
  # diese Daten liegen am obere Rand
  upBInd =  (Data > (maxData - paretoRadius))
  upR <- as.matrix(2 * maxData - Data[upBInd], ncol = 1)
  #extend data by mirrowing
  DataPlus = as.matrix(c(Data, lowR, upR), 1)
  paretoDensity=rep(0, nKernels)
  Fast=TRUE# only for debugging =FALSE
  if(isTRUE(Fast)){
    paretoDensity=c_pde(kernels_internal, nKernels, paretoRadius,  DataPlus)
  }else{
    for (i in 1:nKernels) {
       lb = kernels_internal[i] - paretoRadius
       ub = kernels_internal[i] + paretoRadius
       isInParetoSphere = (DataPlus >= lb) & (DataPlus <= ub)
       paretoDensity[i] = sum(isInParetoSphere)
    }
  }

  #paretoDensity=c_pde_parallel(kernels_internal, nKernels, paretoRadius,  DataPlus)
  
  # for (i in 1:nKernels) {
  #   lb = kernels_internal[i] - paretoRadius
  #   ub = kernels_internal[i] + paretoRadius
  #   isInParetoSphere = (DataPlus >= lb) & (DataPlus <= ub)
  #   paretoDensity[i] = sum(isInParetoSphere)
  # }
  # print(paretoDensity-paretoDensity2)
  # 
  if(requireNamespace('pracma',quietly = TRUE)){ #fuer trapz
		area <- pracma::trapz(kernels_internal, paretoDensity)
  }else{
    if(isFALSE(Silent))
     warning("ParetoDensityEstimation requires the package (pracma) specified in suggest to be installed. Please install this package. Beware: PDE is now not normalized!")
     area=1
  }
  #adhoc numerical calc (not preferable)
  #idx = 2:length(kernels_internal)
  #area <- (as.double((kernels_internal[idx] - kernels_internal[idx - 1]) %*% (paretoDensity[idx] + paretoDensity[idx - 1]))/2)
  
  #Fall kernel==0 => area==NAN muss abgefangen werden, passiert vermutlich nur bei unique values <2
  if (area < 0.0000000001 || is.na(area)) {
    paretoDensity <- rep(0, nKernels)
  } else{
    paretoDensity <- paretoDensity / area
  }
  ##Update 2022 Mai: Schritt 2----
  # nun falls user kernels vorgegeben hat, approximiere an diesen stellen

  if(length(kernels)>1){
    paretoDensity_internal=paretoDensity
    
      if((kernels[1]-paretoRadius)!=minData){#aber ueble warneldung vorgeben!
        if(isFALSE(Silent))
        message("ParetoDensityEstimation(): range of kernels is higher than minimum of data. Density estimation is incomplete. Please either set kernels correctly or let the function set the kernels automatically!")
      }
      if((tail(kernels,1)+paretoRadius)!=maxData){
        if(isFALSE(Silent))
          message("ParetoDensityEstimation(): range of kernels is lower than maximum of data. Density estimation is incomplete Please either set kernels correctly or let the function set the kernels automatically!")
      }
    
    paretoDensity=stats::approx(kernels_internal, paretoDensity, xout =kernels,rule = 1, ties = "ordered")$y
    paretoDensity[!is.finite(paretoDensity)]=0#da wo wir in daten keine dichte geschaetzt haben, ist die dichte null
  }else{
    kernels=kernels_internal
    kernels_internal=NULL
    paretoDensity_internal=NULL
  }
  if (isTRUE(PlotIt)) {
    
    plot(
      kernels,
      paretoDensity,
      type = 'l',
      main = 'Raw PDE R plot',
      xaxs = 'i',
      yaxs = 'i',
      xlab = xlab,
      ylab = 'PDE',
      ylim=c(0,max(paretoDensity)*1.1)
    )
  }
  return (list(
    kernels = kernels,
    paretoDensity = paretoDensity,
    paretoRadius = paretoRadius,
    kernels_internal=kernels,
    paretoDensity_internal=paretoDensity_internal
  ))
  
}

