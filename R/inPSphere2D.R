inPSphere2D = function(data,paretoRadius=NULL){
#   determine the data points inside a ParetoSphere with ParetoRadius
#   INPUT
#   Data[d,2]                the data points, Data = [x,y] 
#   OPTIONAL
#   ParetoRadius             radius of P-spherses; calculated if not given
#  
#   OUTPUT
#   IsInSphere[d,1]         binary vector: IsInSphere(i) <=> Data[i,] is in (at least) one P-sphere
#   InSphereInd              index vector of those Data points in at least one P-sphere 
#   AnzInPspheres[d,1]       number of Data points inside a P-sphere with ParetoRadius around Kernels 
#  
# 
#   MT 03/2016
# Implementation der Hauptschleife in C++: FP 03/2016

	if(is.null(paretoRadius))
		paretoRadius = ParetoRadius(data)

	nData = nrow(data)
	nVar = ncol(data)
	
	x = data[,1]
	y = data[,2]

#  remove NaN from Data
	noNaNInd = (!is.nan(x)&!is.nan(y))
	x = x[noNaNInd]
	y = y[noNaNInd]

	xMin = min(x)
	xMax = max(x)
	yMin = min(y)
	yMax = max(y)
	
	xBinWidth = paretoRadius
	yBinWidth = paretoRadius
#  Kachel Grenzen ausrechenen
	xedge = seq(xMin,(xMax+xBinWidth),by=xBinWidth)
	yedge = seq(yMin,(yMax+yBinWidth),by=yBinWidth)

#  Inhalte der Kacheln ausrechnen

	#see if range fits, extent if necessary
	if(min(x)<xedge[1])	
		xedge = rbind(min(x),xedge)
	if(max(x)>xedge[length(xedge)])	
		xedge = rbind(xedge,max(x))

	#see if range fits, extent if necessary
	if(min(y)<yedge[1])	
		yedge = rbind(min(y),yedge)
	if(max(y)>yedge[length(yedge)])	
		yedge = rbind(yedge,max(y))

	e = hist(x,xedge,plot=FALSE)
	xBinNr = findInterval(x,e$breaks)
	
	e = hist(y,yedge,plot=FALSE)
	yBinNr = findInterval(y,e$breaks)
	
	nrXBins = length(xedge)
	nrYBins = length(yedge)
	
	nInPsphere = c_inPSphere2D(data, xBinNr, yBinNr, nrXBins, nrYBins, nData, paretoRadius)
  return (nInPsphere) 

}

