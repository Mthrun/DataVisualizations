PDEnormrobust <- function(Data,xlab='PDE',ylab,main='PDEnormrobust',
                          PlotSymbolPDE='blue',
                          PlotSymbolGauss= 'magenta',
                          PlotIt=TRUE,Mark2Sigma=FALSE,Mark3Sigma=FALSE,p_mean=10,p_sd=25,...){
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
# Mark2Sigma               TRUE: sets to vertical lines marking data outside M+-1.96SD 
# Mark3Sigma               TRUE: sets to vertical lines marking data outside M+-2.576SD 
# p_mean                  scalar between 1-99, percent of the top- and bottomcut from x
# p_sd                    scalar between 1-99, lowInnerPercentile for robustly estimated standard deviation
# OUTPUT a list of
# Kernels            the x points of the PDE function
# ParetoDensity      the PDE(x)
# ParetoRadius       the ParetoRadius used for the plot
# Pars               Named vector of robustly estimatated mean, standard deviation, 1.96*SD and 2.576*SD

		
pdeVal <- ParetoDensityEstimation(Data)
m <- Meanrobust(Data,p = p_mean/100)
s <- Stdrobust(Data,lowInnerPercentile = p_sd)

if(missing(ylab)){
  ylab=paste('bl=PDE, mg=N(',round(m,1),',',round(s,1),')')
}

normaldist <- dnorm(pdeVal$kernels,m,s) #the Gaussian with the empirical parametrers

	if(PlotIt){
		plot(pdeVal$kernels,pdeVal$paretoDensity,col=PlotSymbolPDE,type='l',xlab=xlab,ylab=ylab,main=main,...)
		points(pdeVal$kernels,normaldist,col=PlotSymbolGauss,type='l',...)
		if(isTRUE(Mark2Sigma)){
		  abline(v=m+1.96*s,col="red",lwd=2)
		  abline(v=m-1.96*s,col="red",lwd=2)
		}
		if(isTRUE(Mark3Sigma)){
		  abline(v=m+2.576*s,col="red",lwd=2)
		  abline(v=m-2.576*s,col="red",lwd=2)
		}
	}


ind_sigma2=which(pdeVal$kernels<=m+1.96*s &pdeVal$kernels>=m-1.96*s)
area_sigma2 <- pracma::trapz(pdeVal$kernels[ind_sigma2], pdeVal$paretoDensity[ind_sigma2])

ind_sigma3=which(pdeVal$kernels<=m+2.576*s &pdeVal$kernels>=m-2.576*s)
area_sigma3 <- pracma::trapz(pdeVal$kernels[ind_sigma3], pdeVal$paretoDensity[ind_sigma3])

Pars=c(m,s,s*1.96,s*2.576,area_sigma2*100,area_sigma3*100)
names(Pars)=c("Mean","SD","Sgima2","Sigma3","EstPercData_Sigma2","EstPercData_Sigma3")

invisible(list(Kernels=pdeVal$kernels,ParetoDensity=pdeVal$paretoDensity,
               ParetoRadius=pdeVal$paretoRadius,Normaldist=normaldist,Pars=Pars)) 

}# end function pdenormrobust

