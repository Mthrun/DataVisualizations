PDEplot <- function(Data,paretoRadius=0,weight=1,kernels=NULL,LogPlot=F,PlotIt=TRUE,title="ParetoDensityEstimation(PDE)",color="blue",xpoints=FALSE, xlim,ylim,xlab,ylab ='PDE',ggPlot=ggplot(),sampleSize=200000,lwd=2)
{
#plot ParetoDensityEstimation (PDE) and Gaussian with empirical Mean and  Variance
#E <-  PDEplot(Data,paretoRadius,weight,kernels,ylabel,plot,title,color,xpoints,defaultAxis, xlim,ylim ,xlab,ylab,PlotSymbolPDE=color);
#Kernels        <- E$kernels;
#ParetoDensity  <- E$paretoDensity;
#ParetoRadius   <- E$paretoRadius;
#ParetoPlot     <- E$plot;
#
# INPUT
# Data[1:n]               Vector of Data to be plotted
# 
# OPTIONAL
# paretoRadius            the Pareto Radius, if omitted, ==0 or ==NaN then ParetoRadius = ParetoRadius(Data);
# weight                  plot PDE(Data)*weight, default weight=1
# kernel                  the x points of the density plots if kernels==0, these are estimated trough ParetoDensityEstimation(Data,paretoRadius,kernels)
# PlotIt                    if not plot==TRUE no plot is done (deprecated)
# LogPlot                 LogLog PDEplot if TRUE
# title                   label for the title  of the plot 
# color                   character vector, color of plot
# gPlot                   ggplot2 object to be plotted upon. Insert an exisiting plot to add a new PDEPlot to it. Default: empty plot
  # sampleSize              default(100000), sample size, if datavector is to big
  
# OUTPUT a list of
# kernels            the x points of the PDE function
# paretoDensity      the PDE(x)
# paretoRadius       the ParetoRadius used for the plot
# plot               ggplot2 object. Can be used to further modify the plot or add other plots.  
  
# uses ParetoDensityEstimation()
# uses ParetoRadius()
#Example:
#  ggplotObj=ggplot()
#  for(i in 1:length(Header))
#     ggplotObj=PDEplot(Data[,i],gPlot = ggplotObj)$plot
  
# ALU 2014 
# 1. Editor: MT 11/1014
# angepasst an Mdbt und Doku standards
# ergaenzt um Parameter PlotSymbolPDE
# 2. Editor: FP 12/2015
# Umstieg auf ggplot
#  require('ggplot2')
#  require('reshape2')
# 3.Editor: MT: Fehlerabfang fuer ggobject, sampelsize
  # Daten normalisieren

	
  if(missing(xlab)) xlab=deparse1(substitute(Data))
  Data=checkFeature(Data,varname='Data',Funname="PDEplot")
	  
  isnumber=function(x) return(is.numeric(x)&length(x)==1)
  
  if(!isnumber(weight))
    stop('"weight" is not a numeric number of length 1. Please change Input.')
  
  if(!isnumber(paretoRadius))
    stop('"paretoRadius" is not a numeric number of length 1. Please change Input.')
  if(!isnumber(lwd))
    stop('"lwd" is not a numeric number of length 1. Please change Input.')
  if(!isnumber(sampleSize))
    stop('"sampleSize" is not a numeric number of length 1. Please change Input.')
  
  if(length(Data)>sampleSize){
    ind=sample(1:length(Data),size = sampleSize)
    Data=Data[ind]
  }

  if(length(paretoRadius)<1 || is.nan(paretoRadius)  || (paretoRadius==0))
    pdeVal        <- ParetoDensityEstimation(Data,NULL,kernels)
  else
    pdeVal        <- ParetoDensityEstimation(Data,paretoRadius,kernels)
 
  paretoDensity <- pdeVal$paretoDensity*weight

  df = data.frame(kernels = pdeVal$kernels, density = paretoDensity)

  plt <- ggPlot
  if(length(grep(pattern = 'gg',attributes(ggPlot)$class))<1){
    warning('ggobject of Inputparameter ggPlot, could not be found. Creating new object.')
    plt=ggplot2::ggplot()
  }
  if(isTRUE(xpoints)){
    plt <- plt + ggplot2::geom_point(data = df, ggplot2::aes(x = .data$kernels, y = .data$density), colour = color)
  }else{
    plt <- plt + ggplot2::geom_line(data = df, ggplot2::aes(x = .data$kernels, y = .data$density), colour = color,size=lwd)  
  }
  plt <- plt  + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::ggtitle(title)+ggplot2::ylim(c(0,1.05*max(df$density)))

  if(isTRUE(LogPlot)){
    plt <- plt + scale_x_log10() + scale_y_log10()
  }#LogPlot
  if(!missing(xlim)){ plt <- plt +ggplot2::coord_cartesian(xlim=xlim)}
  if(!missing(ylim)){ plt <- plt + ggplot2::coord_cartesian(ylim=ylim)}
  ## Alterantiv zum oben genannten: + xlim(xlim) bzw ylim(ylim)
  ##  Das macht allerdings alle Werte ausserhalb des Bereichs zu NA
  if(isTRUE(PlotIt)){
    print(plt)
  }#plot


  invisible(list(kernels=pdeVal$kernels,paretoDensity=pdeVal$paretoDensity,paretoRadius=paretoRadius, ggPlot = plt))#MT: Output wird nicht geprintet und nur ausgegeben, falls Vairable gesetzt
}
