PDEnormrobust <- function(Data,xlab='PDE',ylab,main='PDEnormrobust',
                          PlotSymbolPDE='blue',PlotSymbolGauss= 'magenta',PlotIt=TRUE,...){
# plot ParetoDensityEstimation (PDE) and Gaussian with empirical Mean and  Variance, robust estimation
# V <-  PDEnorm(Data,ParetoRadius,ylabel, main,PlotSymbolPDE,PlotSymbolGauss);
#Kernels        <- V$Kernels;
#ParetoDensity  <- V$ParetoDensity;
#ParetoRadius   <- V$ParetoRadius;
#
# INPUT
# Data[1:n]               Vector of Data to be plotted
# 
# OPTIONAL
# ParetoRadius            the Pareto Radius, if omitted, ==0 or ==NaN then ParetoRadius = ParetoRadius(Data);
# xlabel                  label for the x-Axis of the plot 
# main                   label for the main  of the plot 
# PlotSymbolPDE           color for plotting PDE       see Function plot(...col=PlotSymbolPDE), 'blue' if omitted
# PlotSymbolGauss         color for plotting Gaussian  see Function plot(...col=PlotSymbolPDE), 'magenta' if omitted
#  
# 
# OUTPUT a list of
# Kernels            the x points of the PDE function
# ParetoDensity      the PDE(x)
# ParetoRadius       the ParetoRadius used for the plot

Data <- as.matrix(Data);	nRow <- nrow(Data); nCol <- ncol(Data)
if(nCol>nRow)	Data <- t(Data)
		

pdeVal <- ParetoDensityEstimation(Data)
m <- Meanrobust(Data)
s <- Stdrobust(Data)

if(missing(ylab)){
  ylab=paste('bl=PDE, mg=N(',round(m,1),',',round(s,1),')')
}

normaldist <- dnorm(pdeVal$kernels,m,s) #the Gaussian with the empirical parametrers

	if(PlotIt){
		plot(pdeVal$kernels,pdeVal$paretoDensity,col=PlotSymbolPDE,type='l',xlab=xlab,ylab=ylab,main=main,...)
		points(pdeVal$kernels,normaldist,col=PlotSymbolGauss,type='l')
		
	}

invisible(list(Kernels=pdeVal$kernels,ParetoDensity=pdeVal$paretoDensity, ParetoRadius=pdeVal$paretoRadius,Normaldist=normaldist)) 

 }# end function pdenormrobust

