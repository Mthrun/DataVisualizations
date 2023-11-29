# function [Kernels,ClassParetoDensities] = ClassPDEplotMaxLikeli(Data,Cls,ColorSequence,ColorSymbSequence,PlotLegend,MinAnzKernels,PlotNorm);
ClassPDEplotMaxLikeli <- function(Data, Cls, ColorSequence = DataVisualizations::DefaultColorSequence, ClassNames = NULL, PlotLegend=TRUE, MinAnzKernels=0,PlotNorm=0,main='Pareto Density Estimation (PDE)', xlab='Data',ylab='ParetoDensity', xlim = NULL, ylim = NULL, lwd=1,...){
# res=ClassPDEplotMaxLikeli(Data, Cls)
# PDEplot the data for allclasses, weight the Plot with 1 (= maximum likelihood)
# INPUT
# Data                 the Data to be plotted
# Cls                  vector of class identifiers can be integers or
#                     NaN's, need not be consecutive nor positive
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
  if(!is.vector(Data)){
    warning('Data is expected to me a vector. Calling as.vector().')
    Data=as.vector(Data)
  }
  
  requireNamespace('reshape2')
  if(MinAnzKernels <= 0) MinAnzKernels=100

  Out = Data
  NoNanInd <- which(!is.nan(Data))
  Data <- Data[NoNanInd]
  Cls <- Cls[NoNanInd]
  
  AnzData = length(Data)
  Cls=checkCls(Cls,AnzData)
  #ClCou <- ClassCount(Cls)
  UniqueClasses = sort(unique(Cls),decreasing = F,na.last = T)#ClCou$UniqueClasses
  #CountPerClass = #ClCou$CountPerClass
    NrOfClasses = length(UniqueClasses)#ClCou$NumberOfClasses
  
  CountPerClass <- rep(0, NrOfClasses)
  for (i in 1:NrOfClasses) {
    inClassI <- sum(Cls == UniqueClasses[i])
    CountPerClass[i] = inClassI
    ClassPercentages=inClassI/length(Cls) * 100
  }
  
  #ClassPercentages = ClCou$ClassPercentages # KlassenZaehlen 

  PDEP = ParetoDensityEstimation(Data=Data,paretoRadius=0,kernels=0,MinAnzKernels)
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

    pdeVal <- ParetoDensityEstimation(Data[ClassInd], paretoRadius=ParetoRadiusGesamt, kernels=Kernels)

    Kernels = pdeVal$kernels
    ParetoDensity = pdeVal$paretoDensity

    #if(is.null(dim(ClassParetoDensities))){
     # ClassParetoDensities = ParetoDensity
    #}else{
      #ClassParetoDensities[,c] = ParetoDensity
      ClassParetoDensitiesL[[c]]=ParetoDensity
    #}
  }
  ClassParetoDensities=do.call(CombineCols,ClassParetoDensitiesL)
  
  ClassParetoDensities=ClassParetoDensities[1:length(Kernels),]
  ClassParetoDensities[is.na(ClassParetoDensities)]=0
    for(c in 1:NrOfClasses){
    if(PlotNorm==1){
    M = mean(Data[ClassInd],na.rm=T) #% empirical Mean
    S = sd(Data[ClassInd],na.rm=T)  # empirical Sdev
    Normaldist[,c] = dnorm(Kernels,M,S) # the Gaussian with the empirical parametrers
   # plot(Kernels,Normaldist,PlotSymbolGauss)
    } #    if PlotNorm==1
    if(PlotNorm==2){
      M=mean(Data[ClassInd], trim = 0.1, na.rm = TRUE)

      S=Stdrobust(Data[ClassInd])
    Normaldist[,c] = dnorm(Kernels,M,S) # the Gaussian with the empirical parametrers
    #plot(Kernels,Normaldist,PlotSymbolGauss)
    }#   if PlotNorm==2
  }

  #if(is.null(xlim))
  #  xlim=c(min(Kernels,na.rm=TRUE),max(Kernels,na.rm=TRUE))
  #if(is.null(ylim))
  #  ylim=c(0,max(ClassParetoDensities))

  xlength = abs(min(Kernels,na.rm=TRUE) - max(Kernels,na.rm=TRUE))
  ylength = max(ClassParetoDensities)
  if(is.null(ClassNames)){
    ClassNames = c(1:NrOfClasses)
    ClassNames <- paste("C", ClassNames, sep = "")
  }
  if(PlotNorm>0){
    #fuege als dataframe zusammen
    norms = data.frame(Normaldist)
    colnames(norms) <- ClassNames
    norms$kernels = Kernels
    normsm = reshape2::melt(norms, id='kernels')
  }
  cpd = data.frame(ClassParetoDensities)
  colnames(cpd) <- ClassNames
  cpd$kernels = Kernels
  cpdm = reshape2::melt(cpd, id="kernels")
  ind=which(colnames(cpdm)=="value")
  if(length(ind)>0)
    colnames(cpdm)[ind]="PDE"
  else{
    warning('Could not find y values for ggplot')
  }
  plt <- ggplot()
  if(PlotNorm>0){
    plt <- plt + geom_line(data = normsm, mapping = aes(x=.data$kernels, y=.data$value, color=.data$variable), linetype = 1, size = lwd)
  }
  plt <- plt + geom_line(data=cpdm, aes(x=.data$kernels, y=.data$PDE, color=.data$variable),size = lwd)
  plt <- plt + ggtitle(main) +
    theme(plot.title = element_text(lineheight = .8, face="bold"))
  plt <- plt + ylab(ylab) + xlab(xlab)
  plt <- plt + labs(colour = "Classes")
  plt <- plt + coord_fixed(ratio = xlength/ylength)
  plt <- plt + scale_color_manual(values = ColorSequence)

  if(!is.null(xlim))
    plt <- plt + scale_x_continuous(limits = xlim) 
  if(!is.null(ylim))
    plt <- plt + scale_y_continuous(limits = ylim)
  plt

  invisible(list(Kernels=Kernels, ClassParetoDensities=ClassParetoDensities, ggobject=plt,Dataframe=cpdm))
}
