PDEplot <- function(data,paretoRadius=0,weight=1,kernels=NULL,LogPlot=F,PlotIt=TRUE,title="ParetoDensityEstimation(PDE)",color="blue",xpoints=FALSE, xlim,ylim,xlab ='Data',ylab ='PDE',ggPlot=ggplot(),sampleSize=200000,lwd=2)
{
#plot ParetoDensityEstimation (PDE) and Gaussian with empirical Mean and  Variance
#E <-  PDEplot(data,paretoRadius,weight,kernels,ylabel,plot,title,color,xpoints,defaultAxis, xlim,ylim ,xlab,ylab,PlotSymbolPDE=color);
#Kernels        <- E$kernels;
#ParetoDensity  <- E$paretoDensity;
#ParetoRadius   <- E$paretoRadius;
#ParetoPlot     <- E$plot;
#
# INPUT
# data[1:n]               Vector of Data to be plotted
# 
# OPTIONAL
# paretoRadius            the Pareto Radius, if omitted, ==0 or ==NaN then ParetoRadius = ParetoRadius(Data);
# weight                  plot PDE(data)*weight, default weight=1
# kernel                  the x points of the density plots if kernels==0, these are estimated trough ParetoDensityEstimation(data,paretoRadius,kernels)
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
  data=checkFeature(data,'data')
  isnumber=function(x) return(is.numeric(x)&length(x)==1)
  
  if(!isnumber(weight))
    stop('"weight" is not a numeric number of length 1. Please change Input.')
  
  if(!isnumber(paretoRadius))
    stop('"paretoRadius" is not a numeric number of length 1. Please change Input.')
  if(!isnumber(lwd))
    stop('"lwd" is not a numeric number of length 1. Please change Input.')
  if(!isnumber(sampleSize))
    stop('"sampleSize" is not a numeric number of length 1. Please change Input.')
  
  if(length(data)>sampleSize){
    ind=sample(1:length(data),size = sampleSize)
    data=data[ind]
  }

  if(length(paretoRadius)<1 || is.nan(paretoRadius)  || (paretoRadius==0))
    pdeVal        <- ParetoDensityEstimation(data,NULL,kernels)
  else
    pdeVal        <- ParetoDensityEstimation(data,paretoRadius,kernels)
 
  paretoDensity <- pdeVal$paretoDensity*weight

  df = data.frame(kernels = pdeVal$kernels, density = paretoDensity)
  plt <- ggPlot
  if(length(grep(pattern = 'gg',attributes(ggPlot)$class))<1){
    warning('ggobject of Inputparameter ggPlot, could not be found. Creating new object.')
    plt=ggplot()
  }
  if(isTRUE(xpoints)){
    plt <- plt + geom_point(data = df, aes_string(x = "kernels", y = "density"), colour = color)
  }else{
    plt <- plt + geom_line(data = df, aes_string(x = "kernels", y = "density"), colour = color,size=lwd)  
  }
  plt <- plt  + xlab(xlab) + ylab(ylab) + ggtitle(title)

  if(isTRUE(LogPlot)){
    plt <- plt + scale_x_log10() + scale_y_log10()
  }#LogPlot
  if(!missing(xlim)){ plt <- plt + coord_cartesian(xlim=xlim)}
  if(!missing(ylim)){ plt <- plt + coord_cartesian(ylim=ylim)}
  ## Alterantiv zum oben genannten: + xlim(xlim) bzw ylim(ylim)
  ##  Das macht allerdings alle Werte außerhalb des Bereichs zu NA
  if(isTRUE(PlotIt)){
    print(plt)
  }#plot


  invisible(list(kernels=pdeVal$kernels,paretoDensity=pdeVal$paretoDensity,paretoRadius=paretoRadius, ggPlot = plt))#MT: Output wird nicht geprintet und nur ausgegeben, falls Vairable gesetzt
}
