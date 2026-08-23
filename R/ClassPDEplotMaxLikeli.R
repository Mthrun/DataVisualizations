ClassPDEplotMaxLikeli = function(Data, Cls, ColorSequence = DataVisualizations::DefaultColorSequence, ClassNames = NULL, PlotLegend=TRUE, MinAnzKernels=0,PlotNorm=0,main='Pareto Density Estimation (PDE)', xlab='Data',ylab='ParetoDensity', xlim = NULL, ylim = NULL, lwd=1,...){
# res=ClassPDEplotMaxLikeli(Data, Cls)
# Plot unweighted class-conditional PDEs p(x | C); class priors are not applied.
# Comparing curve heights is maximum likelihood and equals MAP only for equal priors.
# INPUT
# Data                 the Data to be plotted
# Cls                  vector of non-missing class identifiers; values
#                     need not be consecutive nor positive
# OPTIONAL
# ColorSequence        the sequence of colors used, if ==0 r not given: DefaultColorSequence
# ClassNames           Vector of classnames to show correct legend
# xlim                 Plotted area of the x-axis
# ylim                 Plotted area of the y-axis
# MinAnzKernels        Minimale Anzahl Kernels, wenn nicht angegeben oder MinAnzKernelss ==0 =>  MinAnzKernels=100;
# PlotLegend           ==1 (default) add a legent to plot
# PlotNorm             ==1 => plot Normal distribuion on top , ==2 = plot robust normal distribution,; default:  PlotNorm= 0
# OUTPUT
# Kernels,ClassParetoDensities         die PDEs
# ggobject                  ggplot2 plot
# 
#  library(reshape2)
#  library(ggplot2)
# author: Felix Pape
  #1.Editor: MT 2018
  #2.editor 08/26 MT, diverse bugfixes
  if(!is.vector(Data)){
    warning('Data is expected to be a vector. Calling as.vector().')
    Data=as.vector(Data)
  }
  # Coerce Data to numeric before finite-value filtering.
  if(!is.numeric(Data)){
    warning('Data is expected to be numeric. Calling as.numeric().')
    Data=as.numeric(Data)
  }
  
  # Check the dependency and validate PlotNorm.
  if(!requireNamespace('reshape2',quietly=TRUE))
    stop("Package 'reshape2' is required.")
  if(MinAnzKernels <= 0) MinAnzKernels=100
  if(!(PlotNorm %in% c(0,1,2))) stop('PlotNorm must be 0, 1, or 2.')

  # Reject missing classes, validate Cls, and remove non-finite data.
  if(anyNA(Cls)) stop('Cls must not contain missing values.')
  Cls=checkCls(Cls,length(Data))
  if(anyNA(Cls)) stop('Cls must remain non-missing after validation.')
  NoNanInd = which(is.finite(Data))
  Data = Data[NoNanInd]
  Cls = Cls[NoNanInd]
  if(length(Data) == 0) stop('Data must contain at least one finite value.')
  
  AnzData = length(Data)
  Cls=checkCls(Cls,AnzData)
  UniqueClasses = sort(unique(Cls),decreasing = F,na.last = T)#ClCou$UniqueClasses
    NrOfClasses = length(UniqueClasses)#ClCou$NumberOfClasses
  
  # Correct the class-percentage vector (retained for compatibility).
  CountPerClass = rep(0, NrOfClasses)
  ClassPercentages = rep(0, NrOfClasses)
  for (i in 1:NrOfClasses) {
    inClassI = sum(Cls == UniqueClasses[i])
    CountPerClass[i] = inClassI
    ClassPercentages[i]=inClassI/length(Cls) * 100
  }

  #  Name MinAnzKernels explicitly.
  PDEP = ParetoDensityEstimation(Data=Data,paretoRadius=0,kernels=0,MinAnzKernels=MinAnzKernels)
  Kernels = PDEP$kernels
  ParetoDensity = PDEP$paretoDensity
  ParetoRadiusGesamt = PDEP$paretoRadius

  #Normaldist=list()
  Normaldist = matrix(data = 0, nrow = length(Kernels), ncol = NrOfClasses)
  
  #ClassParetoDensities = Kernels * matrix(1, length(Kernels), NrOfClasses)#ones(length(Kernels),NrOfClasses)
  ClassParetoDensitiesL=list()
  for(c in 1:NrOfClasses){
    Class = UniqueClasses[c]
    ClassInd = which(Cls==Class)

    pdeVal = ParetoDensityEstimation(Data[ClassInd], paretoRadius=ParetoRadiusGesamt, kernels=Kernels)

    # Keep the pooled common grid and align exceptional results to it.
    ParetoDensity = pdeVal$paretoDensity
    if(length(ParetoDensity) != length(Kernels) || !isTRUE(all.equal(pdeVal$kernels,Kernels)))
      ParetoDensity = stats::approx(pdeVal$kernels,ParetoDensity,xout=Kernels,yleft=0,yright=0)$y

    #if(is.null(dim(ClassParetoDensities))){
     # ClassParetoDensities = ParetoDensity
    #}else{
      #ClassParetoDensities[,c] = ParetoDensity
      ClassParetoDensitiesL[[c]]=ParetoDensity
    #}
  }
  # Combine equal-grid columns with base cbind and retain matrix shape.
  ClassParetoDensities=do.call(cbind,ClassParetoDensitiesL)
  
  ClassParetoDensities=ClassParetoDensities[1:length(Kernels),,drop=FALSE]
  ClassParetoDensities[is.na(ClassParetoDensities)]=0
    for(c in 1:NrOfClasses){
    # Recompute each class index and avoid invalid zero-scale normals.
    ClassInd = which(Cls==UniqueClasses[c])
    if(PlotNorm==1){
    M = mean(Data[ClassInd],na.rm=T) #% empirical Mean
    S = sd(Data[ClassInd],na.rm=T)  # empirical Sdev
    if(is.finite(S) && S > 0)
      Normaldist[,c] = dnorm(Kernels,M,S) # the Gaussian with the empirical parameters
   # plot(Kernels,Normaldist,PlotSymbolGauss)
    } #    if PlotNorm==1
    if(PlotNorm==2){
      M=mean(Data[ClassInd], trim = 0.1, na.rm = TRUE)

      S=Stdrobust(Data[ClassInd])
    if(is.finite(S) && S > 0)
      Normaldist[,c] = dnorm(Kernels,M,S) # the Gaussian with the empirical parameters
    #plot(Kernels,Normaldist,PlotSymbolGauss)
    }#   if PlotNorm==2
  }

  #if(is.null(xlim))
  #  xlim=c(min(Kernels,na.rm=TRUE),max(Kernels,na.rm=TRUE))
  #if(is.null(ylim))
  #  ylim=c(0,max(ClassParetoDensities))

  xlength = abs(min(Kernels,na.rm=TRUE) - max(Kernels,na.rm=TRUE))
  ylength = max(ClassParetoDensities)
  # Use class identifiers by default and validate custom names.
  if(is.null(ClassNames)){
    ClassNames = as.character(UniqueClasses)
  }
  if(length(ClassNames) != NrOfClasses)
    stop('ClassNames must contain exactly one name per class.')
  if(PlotNorm>0){
    #fuege als dataframe zusammen
    norms = data.frame(Normaldist)
    colnames(norms) = ClassNames
    norms$kernels = Kernels
    normsm = reshape2::melt(norms, id='kernels')
  }
  cpd = data.frame(ClassParetoDensities)
  colnames(cpd) = ClassNames
  cpd$kernels = Kernels
  cpdm = reshape2::melt(cpd, id="kernels")
  ind=which(colnames(cpdm)=="value")
  if(length(ind)>0)
    colnames(cpdm)[ind]="PDE"
  else{
    warning('Could not find y values for ggplot')
  }
  plt = ggplot()
  if(PlotNorm>0){
    plt = plt + geom_line(data = normsm, mapping = aes(x=.data$kernels, y=.data$value, color=.data$variable), linetype = 1, linewidth = lwd)
  }
  # Use linewidth and pass ... to the class-density layer.
  plt = plt + geom_line(data=cpdm, aes(x=.data$kernels, y=.data$PDE, color=.data$variable),linewidth = lwd,...)
  plt = plt + ggtitle(main) +
    theme(plot.title = element_text(lineheight = .8, face="bold"))
  plt = plt + ylab(ylab) + xlab(xlab)
  plt = plt + labs(colour = "Classes")
  #plt = plt + coord_fixed(ratio = xlength/ylength)
  plt = plt + scale_color_manual(values = ColorSequence)

  if(!is.null(xlim))
    plt = plt + scale_x_continuous(limits = xlim) 
  if(!is.null(ylim))
    plt = plt + scale_y_continuous(limits = ylim)
  # ∂Make PlotLegend functional.
  if(!isTRUE(as.logical(PlotLegend)))
    plt = plt + theme(legend.position = 'none')
  plt

  invisible(list(Kernels=Kernels, ClassParetoDensities=ClassParetoDensities, ggobject=plt,Dataframe=cpdm))
}
