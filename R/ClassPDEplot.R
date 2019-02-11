ClassPDEplot <- function(Data,Cls,ColorSequence,ColorSymbSequence,PlotLegend =1,SameKernelsAndRadius=0,xlim,ylim,...){
  # PlotHandle = ClassPDEplot(Data,Cls,ColorSequence,ColorSymbSequence,PlotLegend);
  # PDEplot the data for allclasses, weight the Plot with priors
  # INPUT
  # Data[n]                the Data to be plotted
  # Cls[n]                 vector of class identifiers can be integers or
  #                        NaN's, need not be consecutive nor positive
  # OPTIONAL
  # ColorSequence          the sequence of colors used, Default: DefaultColorSequence
  # ColorSymbSequence      the plot symbols used (theoretisch nicht notwendig, da erst wichtig, wenn mehr als 562 Cluster)
  # PlotLegend             ==1 (default) add a legent to plot
  # SameKernelsAndRadius   ==0 (default); fuer jede Verteilung werden Kernels und Radius individuell bestimmt
  # xlim                   Default c(min(Data),max(Data)) of x-axis
  #
  # OUTPUT
  # Kernels,ClassParetoDensities         die PDEs
  
  # USES ClassCount, ClassUniqeAnz, ParetoDensityEstimation, PDEplot, BMClassColor, DefaultColorSequence, DefaultColorSymbSequence
  # author: 04/2015 Rabea Griese, imported from matlab
  # 1.editor: 04/2015 MT, in max/min na.rm=T added, Fehlerabfang, bei CLS, xlim added
  
  #MT
  if(!is.vector(Data)){
    warning('Data is expected to me a vector. Calling as.vector().')
    Data=as.vector(Data)
  }
  Cls=checkCls(Cls,length(Data))
  # n=length(Cls)
  # if(n!=length(Data))
  #   stop('Number of rows differs with length of cls')
  if(missing(xlim))
    xlim <- c(min(Data,na.rm=T),max(Data,na.rm=T))
  if(missing(ColorSequence)){
    ColorSequence= DataVisualizations::DefaultColorSequence[-2] #no yellow
  }
  Ylimes <- 0
  
  if(missing(ColorSymbSequence))
    ColorSymbSequence = c(20, 4, 3, 2, 6, 8, 9, 10, 15, 16,  17)
  
  #ClCou <- ClassCount(Cls)
  UniqueClasses = unique(Cls)#ClCou$UniqueClasses
  CountPerClass = #ClCou$CountPerClass
  NrOfClasses = length(UniqueClasses)#ClCou$NumberOfClasses
  
  CountPerClass <- rep(0, NrOfClasses)
  for (i in 1:NrOfClasses) {
    inClassI <- sum(Cls == UniqueClasses[i])
    CountPerClass[i] = inClassI
    ClassPercentages[i]=inClassI/length(Cls) * 100
  }
  
  #ClassPercentages = ClCou$ClassPercentages # KlassenZaehlen 
  #BMClassPlotSymbols =  BMClassColor(NrOfClasses,ColorSequence,ColorSymbSequence); # Klassenfarben erzeugen
  
  if(SameKernelsAndRadius ==1){ # gleiche Kernels und ParetoRadius benutzen fuer alle Verteilungen
    #CUC <- ClassUniqeAnz(Data,Cls)
    UniqueClasses <- UniqueClasses#CUC$UniqueClasses
    UniqAnzPerClass <- #CUC$UniqAnzPerClass
    UniqAnzPerClass <- rep(0, NrOfClasses)
    for (i in 1:NrOfClasses) {
      UniqAnzPerClass[i]=length(unique(Data[Cls==UniqueClasses[i]]))
    }
    MaxWeight <- max(UniqAnzPerClass)
    MaxWeightInd  = which(UniqAnzPerClass==MaxWeight,arr.ind=TRUE)
    MaxWeightInd = min(MaxWeightInd); # falls es 2 gibt;
    Ind = which(Cls==UniqueClasses[MaxWeightInd],arr.ind=TRUE); # nur die Datenpunkte aus der Verteilung mit maxWeight
    Pde <- ParetoDensityEstimation(Data[Ind]);
    Kernels = Pde$kernels
    ParetoDensity = Pde$paretoDensity
    ParetoRadiusGesamt = Pde$paretoRadius # Kernels und ParetoRadius fuer die Gesamtverteilung
    ClassParetoDensities = matrix(Kernels,ncol=1) %*% (c(1:NrOfClasses) * 0 + 1) #ones(1,NrOfClasses);
    Weight=c(1:UniqueClasses) * 0#zeros(length(UniqueClasses),1)
    for(c in 1:NrOfClasses){
      Class = UniqueClasses[c];
      ClassInd = which(Cls==Class,arr.ind=TRUE);
      Weight[c] = ClassPercentages[c]/100; # gewichtet mit a prioris
      PDEP = ParetoDensityEstimation(Data=Data[ClassInd],paretoRadius=ParetoRadiusGesamt,kernels=Kernels,MinAnzKernels=100)
      ParetoDensity = PDEP$paretoDensity
      ClassParetoDensities[,c] = ParetoDensity;
      if(Ylimes < max(ClassParetoDensities[,c],na.rm=T)*rep(Weight[c]))
        Ylimes = max(ClassParetoDensities[,c],na.rm=T) *rep(Weight[c])    
    }
    #figure()
    for(c in 1:NrOfClasses){
      plot(Kernels,ClassParetoDensities[,c]*rep(Weight[c],nrow(ClassParetoDensities)),ylim=c(0,Ylimes),xlim=xlim,typ='l',col=ColorSequence[c],main="ParetoDensityEstimation(PDE)", xlab='Data', ylab='PDE',xaxs='i',yaxs='i')
      par(new=TRUE); 
    }
    par(new=FALSE); 
    return(invisible(list(Kernels=Kernels, ClassParetoDensities=ClassParetoDensities)))
  }
  
  else{
    kernels <- list()
    paretoDensity <- list()
    for(c in 1:NrOfClasses){
      Class = UniqueClasses[c];
      ClassInd = which(Cls==Class,arr.ind=TRUE);
      if(sum(!is.na(Data[ClassInd]))>0){
        if(length(ClassInd)>1){
          Weight = ClassPercentages[c]/100; # gewichtet mit a prioris
          pdeVal <- ParetoDensityEstimation(Data[ClassInd])
          kernels[[c]] <- pdeVal$kernels
          paretoDensity[[c]] <- pdeVal$paretoDensity*Weight
          if(Ylimes < max(paretoDensity[[c]],na.rm=T))
            Ylimes = max(paretoDensity[[c]],na.rm=T)       
        }
      }
    }
    
   #figure()
    for(c in 1:NrOfClasses){
      if(missing(ylim))
        plot(kernels[[c]],paretoDensity[[c]],ylim=c(0,Ylimes),xlim=xlim,typ='l',col=ColorSequence[c],main="ParetoDensityEstimation(PDE)", xlab='Data', ylab='PDE',xaxs='i',yaxs='i',...)
      else
        plot(kernels[[c]],paretoDensity[[c]],ylim=ylim,xlim=xlim,typ='l',col=ColorSequence[c],main="ParetoDensityEstimation(PDE)", xlab='Data', ylab='PDE',xaxs='i',yaxs='i',...)
      
      par(new=TRUE); 
    }
    par(new=FALSE); 
    return(invisible(list(Kernels=kernels, ClassParetoDensities=paretoDensity)))
  }
  
  if(PlotLegend ==1){
    legend('topright', legend=UniqueClasses,fill=ColorSequence[1:NrOfClasses])#BMClassPlotSymbols)
  }

}
