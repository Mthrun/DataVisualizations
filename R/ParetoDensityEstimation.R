ParetoDensityEstimation = function(Data,paretoRadius,kernels=NULL,MinAnzKernels=100,PlotIt=FALSE,Compute="Cpp",Silent=FALSE){
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
#  Fit                     Data (without reflections), raw paretoCounts, countingKernels, interpolationUsed
#                          countingKernels is NULL when counts belong to the returned kernels.
#                          Fit is NULL for the normalized 1/2-value Dirac representation.
# 
#  Author: MT 2019

###############################################
###############################################
  
  Compute=tolower(Compute)
  
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
    if(isFALSE(Silent))
    message('Not all values are finite. Please check of infinite or missing values.')
    Data = Data[is.finite(Data)]
  }
  Data = Data[is.finite(Data)]
  if (length(Data) == 0)
    warning("ParetoDensityEstimation: Data contains no finite observations.")
  values = unique(Data)
  
  if (length(values) > 2 & length(values) < 5) {
    if(isFALSE(Silent))
    warning('Less than 5 unqiue values for density estimation. Function may not work')
  }
  #FLAG_kernels_manualSet=TRUE
  if (length(values) < 3) {
    if(isFALSE(Silent))
    warning(
      '1 or 2 unique values for density estimation. Normalized numerical Dirac representation(s) is(are) returned. Input of "kernels", "paretoRadius" and "MinAnzKernels" are ignored!'
    )
    
    if (values[1] != 0)
      kernels = seq(from = values[1] * 0.9,
                    to = values[1] * 1.1,
                    by = values[1] * 0.0001)
    else
      kernels = seq(from = values[1] - 0.1,
                    to = values[1] + 0.1,
                    by = 0.0001)
    
    spikeIndex = which.min(abs(kernels - values[1]))
    kernels[spikeIndex] = values[1] # avoid losing the spike through rounding
    
    if (length(values) == 2) {
      if (values[2] != 0)
        kernels2 = seq(from = values[2] * 0.9,
                       to = values[2] * 1.1,
                       by = values[2] * 0.0001)
      else
        kernels2 = seq(from = values[2] - 0.1,
                       to = values[2] + 0.1,
                       by = 0.0001)
      
      
      spikeIndex = which.min(abs(kernels2 - values[2]))
      kernels2[spikeIndex] = values[2]
      
      # Add a zero between the spikes when a midpoint is representable.
      kernels = c(kernels, kernels2, values[1] / 2 + values[2] / 2)
    }
    kernels = sort(unique(kernels))
    spikeIndex = match(values, kernels)
    # Normalize each impulse on the final grid, including overlapping grids.
    # A unit-height spike has trapezoidal area (right - left) / 2.
    spikeArea = (kernels[spikeIndex + 1] - kernels[spikeIndex - 1]) / 2
    probabilities = tabulate(match(Data, values), nbins = length(values)) / length(Data)
    spikeHeight = probabilities / spikeArea
    if (any(!is.finite(spikeHeight)) || any(spikeHeight <= 0))
      stop("ParetoDensityEstimation: numerical Dirac heights cannot be represented on this grid.")
    paretoDensity = rep(0, length(kernels))
    paretoDensity[spikeIndex] = spikeHeight
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
      paretoRadius = 0,
      Fit = NULL
    ))
  }# end if if (length(values) < 3)
  
  if (length(Data) < 10) {
    if(isFALSE(Silent))
    warning('Less than 10 datapoints given, ParetoRadius potientially cannot be calcualted.')
  }
  if (!missing(paretoRadius)) {
    if (length(paretoRadius) == 0) {
      paretoRadius = NULL
    } else if (length(paretoRadius) != 1 || is.complex(paretoRadius)) {
      stop("paretoRadius must be a single real number, NULL or NA.")
    }
  }
  #published method in thrun et al 2020
  paretoRadiusNotMissing=TRUE
  if(length(Data)<5*10^3 | Compute=="r"){#smaller data
    if (missing(paretoRadius)) {#10% or bigger sample is taken
      paretoRadiusNotMissing=FALSE
      paretoRadius = ParetoRadius(Data)
    } else if (is.null(paretoRadius)) {
      paretoRadius = ParetoRadius(Data)
      paretoRadiusNotMissing=FALSE
    } else if (is.na(paretoRadius)) {
      paretoRadius = ParetoRadius(Data)
      paretoRadiusNotMissing=FALSE
    } else if (paretoRadius == 0 || length(paretoRadius) == 0) {
      paretoRadiusNotMissing=FALSE
      paretoRadius = ParetoRadius(Data)
    } else{
      #ToNothing because radius is given by user
    }
  }else{#big data
    if(length(Data)>10^5){#vaeriant 1 is approximation
      if (missing(paretoRadius)) {#multiple small samples are taken
        paretoRadiusNotMissing=FALSE
        paretoRadius = mean(sapply(1:100, function(x) return(DataVisualizations::ParetoRadius_fast(Data,maximumNrSamples = 10000))),na.rm=TRUE)
      } else if (is.null(paretoRadius)) {
        paretoRadiusNotMissing=FALSE
        paretoRadius = mean(sapply(1:100, function(x) return(DataVisualizations::ParetoRadius_fast(Data,maximumNrSamples = 10000))),na.rm=TRUE)
      } else if (is.na(paretoRadius)) {
        paretoRadiusNotMissing=FALSE
        paretoRadius = mean(sapply(1:100, function(x) return(DataVisualizations::ParetoRadius_fast(Data,maximumNrSamples = 10000))),na.rm=TRUE)
      } else if (paretoRadius == 0 || length(paretoRadius) == 0) {
        paretoRadiusNotMissing=FALSE
        paretoRadius = mean(sapply(1:100, function(x) return(DataVisualizations::ParetoRadius_fast(Data,maximumNrSamples = 10000))),na.rm=TRUE)
      } else{
        #ToNothing because radius is given by user
      }
    }else{#variant to is taken N=10000 sample with with fast estimation
      if (missing(paretoRadius)) {#10% or bigger sample is taken
        paretoRadiusNotMissing=FALSE
        paretoRadius = ParetoRadius_fast(Data)
      } else if (is.null(paretoRadius)) {
        paretoRadius = ParetoRadius_fast(Data)
        paretoRadiusNotMissing=FALSE
      } else if (is.na(paretoRadius)) {
        paretoRadius = ParetoRadius_fast(Data)
        paretoRadiusNotMissing=FALSE
      } else if (paretoRadius == 0 || length(paretoRadius) == 0) {
        paretoRadius = ParetoRadius_fast(Data)
        paretoRadiusNotMissing=FALSE
      } else{
        #ToNothing because radius is given by user
      }
    }
  }

  if (!is.numeric(paretoRadius) || is.complex(paretoRadius) || length(paretoRadius) != 1 ||
      !is.finite(paretoRadius) || paretoRadius < 0)
    stop("ParetoDensityEstimation: paretoRadius must be finite and positive.")
  # An estimated zero radius is retried by the existing failsave below.
  minData = min(Data, na.rm = TRUE)
  maxData = max(Data, na.rm = TRUE)
  #Update 2022, Mai ----
  #kernels werden im schritt eins immer manuel berechnet egal was der user vorgibt
  #if (length(kernels) <= 1) { #kernels wurden vom user nicht gesetzt
  #  if (length(kernels) == 0 || (length(kernels) == 1 & kernels == 0)) {
      #MT: Korrektur: statt kernels==0 und im Input Kernels=0
      nBins = OptimalNoBins(Data)
      #MT: MinAnzKernels fehlte
      if (isTRUE(MinAnzKernels == 0)) MinAnzKernels = 100
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

      # A grid problem must not change an already positive Pareto radius.
      if (paretoRadius == 0) {
        if(isFALSE(Silent))
          warning("ParetoDensityEstimation: retrying the zero Pareto radius on the full data sample.")
        paretoRadius = ParetoRadius_fast(Data, maximumNrSamples = length(Data), failsave = TRUE)
        if (length(paretoRadius) != 1 || !is.finite(paretoRadius) || paretoRadius <= 0)
          stop("ParetoDensityEstimation: a positive finite paretoRadius could not be determined.")
      }
      maximumUniformBins = max(1E4, nBins)
      repeat{
        if ((maxData - minData) / paretoRadius > maximumUniformBins || nBins > maximumUniformBins) {
          # Sparse fallback: count-change locations, not a grid across empty gaps.
          gridValues = sort(unique(Data))
          gaps = diff(gridValues)
          isolated = c(TRUE, gaps > 4 * paretoRadius) & c(gaps > 4 * paretoRadius, TRUE)
          diracValues = gridValues[isolated]
          denseValues = gridValues[!isolated]

          # Include the change locations contributed by the existing boundary reflections.
          denseValues = c(denseValues,
                         2 * minData - denseValues[denseValues < minData + paretoRadius],
                         2 * maxData - denseValues[denseValues > maxData - paretoRadius])
          countEdges = c(denseValues - paretoRadius, denseValues + paretoRadius)
          edgePadding = 8 * .Machine$double.eps * pmax(abs(countEdges), paretoRadius)
          if (any(!is.finite(countEdges)) ||
              any(edgePadding >= paretoRadius / 4) ||
              any(!is.finite(diracValues - 2 * paretoRadius)) ||
              any(!is.finite(diracValues + 2 * paretoRadius)) ||
              any(diracValues - 2 * paretoRadius >= diracValues) ||
              any(diracValues + 2 * paretoRadius <= diracValues))
            warning("ParetoDensityEstimation: the radius cannot be resolved at this numerical data scale, please provide kernels manually.")

          # An isolated count m gets a triangle of half-width 2*r: raw area 2*r*m.
          # At a global boundary, mirroring doubles m and only half the triangle remains.
          # This matches the integrated count of a width-2*r Pareto window.
          # Dense parts use points on both sides of every count change; outside-support
          # points have zero counts, so interpolation cannot bridge a large empty gap.
          kernels_internal = sort(unique(c(minData, maxData,
                                            diracValues - 2 * paretoRadius, diracValues,
                                            diracValues + 2 * paretoRadius,
                                            denseValues, countEdges - edgePadding,
                                            countEdges + edgePadding)))
          kernels_internal = kernels_internal[kernels_internal >= minData & kernels_internal <= maxData]
          if (length(kernels_internal) < 2 || any(!is.finite(kernels_internal)))
            warning("ParetoDensityEstimation: a finite sparse grid could not be constructed, please provide kernels manually.")
          if(isFALSE(Silent))
            warning("ParetoDensityEstimation: using a gap-aware internal grid; paretoRadius is unchanged.")
          break
        }
        breaks = pretty(c(minData, maxData), n = nBins, min.n = 1)
        nB = length(breaks)
        mids = 0.5 * (breaks[-1L] + breaks[-nB])
        kernels_internal = mids
        if (mean(diff(kernels_internal)) > paretoRadius){
          nBins=nBins+10
        }else{
          break
        }
      }
      #FLAG_kernels_manualSet=FALSE
  #  }
  #}
  #bugfix: MT 2020
  #sicherstellen das alle daten auch in einer ParetoKugel enthalten sind
  #if(isFALSE(FLAG_kernels_manualSet)){
    if((kernels_internal[1]-paretoRadius)>minData){
      kernels_internal=c(minData,kernels_internal)
    } 
    if((tail(kernels_internal,1)+paretoRadius)<maxData){
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

  switch (Compute,
    r = {
      for (i in 1:nKernels) {
        lb = kernels_internal[i] - paretoRadius
        ub = kernels_internal[i] + paretoRadius
        isInParetoSphere = (DataPlus >= lb) & (DataPlus <= ub)
        paretoDensity[i] = sum(isInParetoSphere)
      }
    },
    cpp={
      paretoDensity=c_pde(kernels_internal, nKernels, paretoRadius,  DataPlus)
    },
    cpp_exp={
      paretoDensity=c_pde_fast(kernels_internal, nKernels, paretoRadius,  DataPlus)
    },
    {#default
      paretoDensity=c_pde(kernels_internal, nKernels, paretoRadius,  DataPlus)
    }
  )
  paretoCounts = as.numeric(paretoDensity) # save counts before normalization/interpolation
  # print(paretoDensity)
  # if(sum(paretoDensity,na.rm = T)==0&isFALSE(failsave)){
  #   if(isTRUE(paretoRadiusNotMissing)){
  #     
  #     if(length(Data)>10^5){
  #       warning("ParetoDensityEstimation: failsave activated to measure density, computing pareto radius on large data sample.")
  #       paretoRadius = mean(sapply(1:100, function(x) return(DataVisualizations::ParetoRadius_fast(Data,maximumNrSamples = 25000,failsave=TRUE))),na.rm=TRUE)
  #     }else{
  #       warning("ParetoDensityEstimation: failsave activated to measure density, computing pareto radius on full data without taking a sample.")
  #       paretoRadius = DataVisualizations::ParetoRadius_fast(Data,maximumNrSamples=length(Data),failsave=TRUE)
  #     }
  #     
  #     #do only once
  #     V=ParetoDensityEstimation(Data=Data,paretoRadius=paretoRadius,kernels=kernels_internal,PlotIt=PlotIt,Compute=Compute,Silent=Silent,failsave=TRUE)
  #     paretoDensity=V$paretoDensity_internal
  #     paretoRadius=V$paretoRadius
  #     
  #   }
  # }
 
  if(requireNamespace('pracma',quietly = TRUE)){ #fuer trapz
		area <- pracma::trapz(kernels_internal, paretoDensity)
  }else{
    area <- sum(diff(kernels_internal) *
                (head(paretoDensity, -1) + tail(paretoDensity, -1)) / 2)
  }
  #adhoc numerical calc (not preferable)
  #idx = 2:length(kernels_internal)
  #area <- (as.double((kernels_internal[idx] - kernels_internal[idx - 1]) %*% (paretoDensity[idx] + paretoDensity[idx - 1]))/2)
  
  #Fall kernel==0 => area==NAN muss abgefangen werden, passiert vermutlich nur bei unique values <2
  if (!is.finite(area) || area <= 0) {
    paretoDensity <- rep(0, nKernels)
  } else{
    paretoDensity <- paretoDensity / area
  }
  ##Update 2022 Mai: Schritt 2----
  # nun falls user kernels vorgegeben hat, approximiere an diesen stellen

  interpolationUsed = length(kernels)>1 || (length(kernels)==1 && isTRUE(kernels != 0))
  countingKernels = NULL
  if(interpolationUsed){
    paretoDensity_internal=paretoDensity
    if (length(kernels) != length(kernels_internal) || !isTRUE(all(kernels == kernels_internal)))
      countingKernels = kernels_internal
    
      if((min(kernels,na.rm=TRUE)-paretoRadius)>minData){#aber ueble warneldung vorgeben!
        if(isFALSE(Silent))
        message("ParetoDensityEstimation(): range of kernels is higher than minimum of data. Density estimation is incomplete. Please either set kernels correctly or let the function set the kernels automatically!")
      }
      if((max(kernels,na.rm=TRUE)+paretoRadius)<maxData){
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
  if(isTRUE(PlotIt)){
    plot(kernels, paretoDensity, type = 'l', main = 'Raw PDE R plot', 
         yaxs = 'i', xlab = xlab, ylab = 'PDE',
         ylim=c(0,max(paretoDensity)*1.1),lwd=2)
  }
  Fit = list(
    Data = Data,
    paretoCounts = paretoCounts,
    countingKernels = countingKernels,
    interpolationUsed = interpolationUsed
  )
  return(list(kernels = kernels,
              paretoDensity = paretoDensity,
              paretoRadius = paretoRadius,
              kernels_internal=kernels_internal,
              paretoDensity_internal=paretoDensity_internal,
              Fit=Fit))
}
