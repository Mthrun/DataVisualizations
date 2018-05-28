# function [Kernels,ClassParetoDensities] = ClassPDEplotMaxLikeli(Data,Cls,ColorSequence,ColorSymbSequence,PlotLegend,MinAnzKernels,PlotNorm);
ClassPDEplotMaxLikeli <- function(Data, Cls, ColorSequence = DataVisualizations::DefaultColorSequence, ClassNames = NULL, PlotLegend=TRUE, MinAnzKernels=0,PlotNorm=0,main='Pareto Density Estimation (PDE)', xlab='Data',ylab='ParetoDensity', xlim = NULL, ylim = NULL, ...){
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
  requireNamespace('reshape2')
  if(MinAnzKernels <= 0) MinAnzKernels=100

  AnzData = length(Data)


  Out = Data
  NoNanInd <- which(!is.nan(Data))
  Data <- Data[NoNanInd]
  Cls <- Cls[NoNanInd]

  #ClCou <- ClassCount(Cls)
  UniqueClasses = unique(Cls)#ClCou$UniqueClasses
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

  ClassParetoDensities = Kernels * matrix(1, length(Kernels), NrOfClasses)#ones(length(Kernels),NrOfClasses)

  #Normaldist=list()
  Normaldist = matrix(data = 0, nrow = length(Kernels), ncol = NrOfClasses)
  
  prctile=function (x, p) 
  {
    if (length(p) == 1) {
      if (p > 1) {
        p = p/100
      }
    }
    if (is.matrix(x) && ncol(x) > 1) {
      cols <- ncol(x)
      quants <- matrix(0, nrow = length(p), ncol = cols)
      for (i in 1:cols) {
        quants[, i] <- quantile(x[, i], probs = p, type = 5, 
                                na.rm = TRUE)
      }
    }
    else {
      quants <- quantile(x, p, type = 5, na.rm = TRUE)
    }
    return(quants)
  }
  
  for(c in 1:NrOfClasses){
    Class = UniqueClasses[c]
    ClassInd = which(Cls==Class)

    pdeVal <- AdaptGauss::ParetoDensityEstimation(Data[ClassInd], paretoRadius=ParetoRadiusGesamt, kernels=Kernels)

    Kernels = pdeVal$kernels
    ParetoDensity = pdeVal$paretoDensity

    if(is.null(dim(ClassParetoDensities))) ClassParetoDensities = ParetoDensity
    else ClassParetoDensities[,c] = ParetoDensity
    if(PlotNorm==1){
    M = mean(Data[ClassInd],na.rm=T) #% empirical Mean
    S = sd(Data[ClassInd],na.rm=T)  # empirical Sdev
    Normaldist[,c] = dnorm(Kernels,M,S) # the Gaussian with the empirical parametrers
   # plot(Kernels,Normaldist,PlotSymbolGauss)
    } #    if PlotNorm==1
    if(PlotNorm==2){
      M=mean(Data[ClassInd], trim = 0.1, na.rm = TRUE)
    #M = dbt.Statistics::meanrobust(Data[ClassInd]) # empirical Mean
      stdrobust = function (x, lowInnerPercentile = 25) 
      {
        if (is.vector(x) || (is.matrix(x) && dim(x)[1] == 1)) 
          dim(x) <- c(length(x), 1)
        lowInnerPercentile <- max(1, min(lowInnerPercentile, 49))
        hiInnerPercentile <- 100 - lowInnerPercentile
        faktor <- sum(abs(qnorm(t(c(lowInnerPercentile, hiInnerPercentile)/100), 
                                0, 1)))
        std <- sd(x, na.rm = TRUE)
        p <- c(lowInnerPercentile, hiInnerPercentile)/100
        quartile <- prctile(x, p)
        if (ncol(x) > 1) 
          iqr <- quartile[2, ] - quartile[1, ]
        else iqr <- quartile[2] - quartile[1]
        shat <- c()
        for (i in 1:ncol(x)) {
          shat[i] <- min(std[i], iqr[i]/faktor, na.rm = TRUE)
        }
        dim(shat) <- c(1, ncol(x))
        colnames(shat) <- colnames(x)
        return(shat)
      }
    #S = dbt.Statistics::stdrobust(Data[ClassInd])  # empirical Sdev
      S=stdrobust(Data[ClassInd])
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
  plt <- ggplot()
  if(PlotNorm>0){
    plt <- plt + geom_line(data = normsm, mapping = aes_string(x='kernels', y='value', color='variable'), linetype = 1, size = 1.5)
  }
  plt <- plt + geom_line(data=cpdm, aes_string(x='kernels', y='value', color='variable'))
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

  invisible(list(Kernels=Kernels, ClassParetoDensities=ClassParetoDensities, ggobject=plt))
}
