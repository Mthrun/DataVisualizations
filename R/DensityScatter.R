DensityScatter=function(X,Y,DensityEstimation="SDH",Type="DDCAL", Plotter = "native",Marginals = FALSE,
                        SampleSize,na.rm=FALSE,
                        
                        xlab, ylab, main="DensityScatter", AddString2lab="",
                        
                        xlim, ylim,NoBinsOrPareto=NULL,...) {

  # DensityScatter
  #  plot the PDE on top of a scatterplot 
  #
  #  INPUT
  #  X[1:n]                  First feature
  #  Y[1:n]                  Second feature
  #  DensityEstimation       Either "PDE" or "SDH"
  #  OPTIONAL
  #  SampleSize              Sample Size in case of big data
  #  na.rm                   Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE
  
  #  ylab                    Label for the Y axis
  #                          Possible values are: native, ggplot, plotly
  #
  #  OUTPUT
  #   X,Y
  #  Densities               Numer of points within the Radiuis or SDH of each point
  #  ParetoRadius            ParetoRadius used for PDEscatter.
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
  isnumber=function(X) return(is.numeric(X)&length(X)==1)
  
  if(missing(xlab)) xlab=deparse1(substitute(X))
  if(missing(ylab)) ylab=deparse1(substitute(Y))
  
  if(AddString2lab!=""){
    xlab=paste0(xlab,AddString2lab)
    ylab=paste0(ylab,AddString2lab)
  }
  
  
  if(missing(xlim))
    xlim = c(min(X,na.rm = T), max(X,na.rm = T))
  if(missing(ylim))
    ylim = c(min(Y,na.rm = T), max(Y,na.rm = T))
  
  if (!requireNamespace('ScatterDensity',quietly = TRUE)) {
    message(
      'Subordinate clustering package (ScatterDensity) is missing. Switching to MASS::kde2d density estimation.
            Please install the package which is defined in "Suggests".'
    )
    Type="native"
  } 
  
  if(Type=="DDCAL"){
    if(DensityEstimation=="SDH")
      SDHorPDE=TRUE
    else
      SDHorPDE=FALSE
    
    if(missing(SampleSize))
      V=ScatterDensity::DensityScatter.DDCAL(X=X, Y=Y, xlab=xlab, ylab=ylab, SDHorPDE = SDHorPDE,
                                       Plotter = "native",Marginals = Marginals, na.rm = na.rm,main = main,...)
    else
      V=ScatterDensity::DensityScatter.DDCAL(X=X, Y=Y, xlab=xlab, ylab=ylab, SDHorPDE = SDHorPDE,
                                             Plotter = "native",Marginals = Marginals,
                                             PDEsample = SampleSize, na.rm = na.rm,main = main,...)
    return(V)
      
  }else{
    calcDens = estimateDensity2D(X,Y,DensityEstimation=DensityEstimation, 
                                  SampleSize=SampleSize, na.rm=na.rm, NoBinsOrPareto=NoBinsOrPareto)
    X = calcDens[[1]]
    Y = calcDens[[2]]
    Densities = calcDens[[3]]
    
    ncolors=1
    ncolors2=1
    colpalette=colorRampPalette(c("navyblue","darkblue",rep("blue",ncolors2),
                                  rep("turquoise",ncolors2),rep("green",ncolors),
                                  rep("chartreuse",ncolors),rep("yellow",ncolors),
                                  rep("orange",ncolors),rep("red",ncolors),
                                  rep("darkred",ncolors2)
    )
    )
    noNaNInd <- is.finite(X) & is.finite(Y)
    
    ##robust normalization does not influence cut
    # quants=quantile(Densities,c(0.001,0.5,0.99))
    # minU=quants[1]
    # maxU=quants[3]
    #Densities4Colors=(Densities-minU)/(maxU-minU)+1
    Densities4Colors=Densities^0.5#stretches range for low densities
    colpal <- cut(Densities4Colors, length(Densities4Colors), labels = FALSE)
    cols <- rep(NA_character_, length(noNaNInd))
    cols[noNaNInd] <- colpalette(length(Densities4Colors))[colpal]
    plot(X[noNaNInd],Y[noNaNInd],col=cols,pch=".",xlim = xlim,
         ylim = ylim,xlab=xlab,ylab=ylab,main=main,...)
  }
  return(invisible(list(X=X,Y=Y,Densities=Densities)))

}
