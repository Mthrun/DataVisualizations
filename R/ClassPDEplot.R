ClassPDEplot = function(Data,Cls,ColorSequence,ColorSymbSequence,PlotLegend =1,SameKernelsAndRadius=0,xlim,ylim,...){
  # PlotHandle = ClassPDEplot(Data,Cls,ColorSequence,ColorSymbSequence,PlotLegend);
  # Plot each class-conditional PDE multiplied by its empirical class prior.
  # The curves are proportional to P(C) * p(x | C), not normalized posteriors.
  # INPUT
  # Data[n]                the Data to be plotted
  # Cls[n]                 vector of non-missing class identifiers; values
  #                        need not be consecutive nor positive
  # OPTIONAL
  # ColorSequence          the sequence of colors used, Default: DefaultColorSequence
  # ColorSymbSequence      the plot symbols used (theoretisch nicht notwendig, da erst wichtig, wenn mehr als 562 Cluster)
  # PlotLegend             ==1 (default) add a legent to plot
  # SameKernelsAndRadius   ==0 (default); fuer jede Verteilung werden Kernels und Radius individuell bestimmt
  # xlim                   Default c(min(Data),max(Data)) of x-axis
  #
  # OUTPUT
  # Kernels,ClassParetoDensities         die PDEs
  
  # USES  ClassUniqeAnz, ParetoDensityEstimation, PDEplot, BMClassColor, DefaultColorSequence, DefaultColorSymbSequence
  # author: 04/2015 Rabea Griese, imported from matlab
  # 1.editor: 04/2015 MT, in max/min na.rm=T added, Fehlerabfang, bei CLS, xlim added
  #2.editor 08/26 MT, diverse bugfixes
  #MT
  if(!is.vector(Data)){
    warning('Data is expected to be a vector. Calling as.vector().')
    Data=as.vector(Data)
  }
  # coerce numeric input and require at least one finite value.
  if(!is.numeric(Data)){
    warning('Data is expected to be numeric. Calling as.numeric().')
    Data=as.numeric(Data)
  }
  if(!any(is.finite(Data)))
    stop('Data must contain at least one finite value.')
  # Reject missing class assignments before checkCls().
  if(anyNA(Cls))
    stop('Cls must not contain missing values.')
  Cls=checkCls(Cls,length(Data))
  if(anyNA(Cls))
    stop('Cls must remain non-missing after validation.')

  if(missing(xlim))
    xlim = range(Data[is.finite(Data)])
  if(missing(ColorSequence)){
    ColorSequence= DataVisualizations::DefaultColorSequence[-2] #no yellow
  }
  Ylimes = 0
  
  if(missing(ColorSymbSequence))
    ColorSymbSequence = c(20, 4, 3, 2, 6, 8, 9, 10, 15, 16,  17)
  
  UniqueClasses = unique(Cls)#ClCou$UniqueClasses
  NrOfClasses = length(UniqueClasses)#ClCou$NumberOfClasses
  ClassPercentages = rep(0, NrOfClasses)
  CountPerClass = rep(0, NrOfClasses)
  for (i in 1:NrOfClasses) {
    inClassI = sum(Cls == UniqueClasses[i])
    CountPerClass[i] = inClassI
    ClassPercentages[i]=inClassI/length(Cls) * 100
  }
  if(SameKernelsAndRadius ==1){ # gleiche Kernels und ParetoRadius benutzen fuer alle Verteilungen
    UniqAnzPerClass = rep(0, NrOfClasses)
    for (i in 1:NrOfClasses) {
      # Count only finite unique values when choosing the shared grid.
      UniqAnzPerClass[i]=length(unique(Data[Cls==UniqueClasses[i] & is.finite(Data)]))
    }
    MaxWeight = max(UniqAnzPerClass)
    MaxWeightInd  = which(UniqAnzPerClass==MaxWeight,arr.ind=TRUE)
    MaxWeightInd = min(MaxWeightInd); # falls es 2 gibt;
    Ind = which(Cls==UniqueClasses[MaxWeightInd],arr.ind=TRUE); # nur die Datenpunkte aus der Verteilung mit maxWeight
    Pde = ParetoDensityEstimation(Data[Ind]);
    Kernels = Pde$kernels
    ParetoDensity = Pde$paretoDensity
    ParetoRadiusGesamt = Pde$paretoRadius # Kernels und ParetoRadius fuer die Gesamtverteilung
    # Initialize density columns with zeros, not kernel values.
    ClassParetoDensities = matrix(0,nrow=length(Kernels),ncol=NrOfClasses)
    # Allocate one weight per class, independent of class values.
    Weight=rep(0, NrOfClasses)
    for(c in 1:NrOfClasses){
      Class = UniqueClasses[c];
      ClassInd = which(Cls==Class,arr.ind=TRUE);
      Weight[c] = ClassPercentages[c]/100; # gewichtet mit a prioris
      # Skip a class that has no finite observations.
      if(!any(is.finite(Data[ClassInd])))
        next
      PDEP = ParetoDensityEstimation(Data=Data[ClassInd],paretoRadius=ParetoRadiusGesamt,kernels=Kernels,MinAnzKernels=100)
      ParetoDensity = PDEP$paretoDensity
      # Align exceptional class estimates to the shared grid.
      if(length(ParetoDensity) != length(Kernels) || !isTRUE(all.equal(PDEP$kernels,Kernels)))
        ParetoDensity = stats::approx(PDEP$kernels,ParetoDensity,xout=Kernels,yleft=0,yright=0)$y
      # Store prior-weighted densities in both kernel modes.
      ClassParetoDensities[,c] = ParetoDensity * Weight[c];
      if(Ylimes < max(ClassParetoDensities[,c],na.rm=T))
        Ylimes = max(ClassParetoDensities[,c],na.rm=T)    
    }
    # Respect ylim and ... in shared-grid mode.
    PlotYlim = if(missing(ylim)) c(0,Ylimes) else ylim
    for(c in 1:NrOfClasses){
      plot(Kernels,ClassParetoDensities[,c],ylim=PlotYlim,xlim=xlim,type='l',col=ColorSequence[c],main="ParetoDensityEstimation(PDE)", xlab='Data', ylab='PDE',xaxs='i',yaxs='i',...)
      par(new=TRUE); 
    }
    par(new=FALSE); 
    # Draw the legend before the branch returns.
    if(PlotLegend ==1){
      legend('topright', legend=UniqueClasses,fill=ColorSequence[1:NrOfClasses])
    }
    return(invisible(list(Kernels=Kernels, ClassParetoDensities=ClassParetoDensities)))
  }
  
  else{
    kernels = list()
    paretoDensity = list()
    for(c in 1:NrOfClasses){
      Class = UniqueClasses[c];
      ClassInd = which(Cls==Class,arr.ind=TRUE);
      if(any(is.finite(Data[ClassInd]))){
        # Allow singleton classes; PDE handles point masses.
        if(length(ClassInd)>0){
          Weight = ClassPercentages[c]/100; # gewichtet mit a prioris
          pdeVal = ParetoDensityEstimation(Data[ClassInd])
          kernels[[c]] = pdeVal$kernels
          paretoDensity[[c]] = pdeVal$paretoDensity*Weight
          if(Ylimes < max(paretoDensity[[c]],na.rm=T))
            Ylimes = max(paretoDensity[[c]],na.rm=T)       
        }
      }
    }
    
   #figure()
    for(c in 1:NrOfClasses){
      # Skip classes for which no density was estimated.
      if(is.null(kernels[[c]]) || is.null(paretoDensity[[c]]))
        next
      if(missing(ylim))
        plot(kernels[[c]],paretoDensity[[c]],ylim=c(0,Ylimes),xlim=xlim,type='l',col=ColorSequence[c],main="ParetoDensityEstimation(PDE)", xlab='Data', ylab='PDE',xaxs='i',yaxs='i',...)
      else
        plot(kernels[[c]],paretoDensity[[c]],ylim=ylim,xlim=xlim,type='l',col=ColorSequence[c],main="ParetoDensityEstimation(PDE)", xlab='Data', ylab='PDE',xaxs='i',yaxs='i',...)
      
      par(new=TRUE); 
    }
    par(new=FALSE); 
    # Draw the legend before the branch returns.
    if(PlotLegend ==1){
      legend('topright', legend=UniqueClasses,fill=ColorSequence[1:NrOfClasses])
    }
    return(invisible(list(Kernels=kernels, ClassParetoDensities=paretoDensity)))
  }
}
