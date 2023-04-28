 SmoothedDensitiesXY=function(X,Y,nbins,lambda,Xkernels,Ykernels,PlotIt=FALSE){
#[Density,Xkernels,Ykernels,F,ind] = SmoothedDensitiesXY(X,Y)
#[Density,Xkernels,Ykernels,F,ind] = SmoothedDensitiesXY(X,Y,nbins,lambda)
# Density is the smothed histogram density at [X,Y],
# F is a discrete mash of densities corresponding to axes kenerls: Xkernels,Ykernels
# if X(i) or Y(i) equals NaN, then  Density(i) = 0
#
# INPUT
# X(1:n),Y(1:n)     a set of 2D points
#
# OPTIONAL
# nbins             number of bins or []
#                   for nbins=[], => nbins =2000 (default)
#                   nbins= nxy      => the nr of bins in x and y is nxy
#                   nbins = [nx,ny] => the nr of bins in x is nx and for y is ny
#
# lambda            smoothing factor used by the density estimator or []
#                   default: lambda = 20 which roughly
#                   means that the smoothing is over 20 bins around a given point.
# Xkernels,Ykernels bin kernels in x and y directions are given
#
# OUTPUT
# Density(1:n)            such that (X,Y,Density) is the smothed density in 3D   max(Density(:)) ==1
# Xkernels,Ykernels,F     such that mesh(Xkernels,Ykernels,F) form the ( not NaN) smothed densisties
# ind                     an index such that Density = F(ind)


# Reference:
# [Eilers/Goeman, 2004]  Eilers, P. H., & Goeman, J. J.: Enhancing scatterplots with smoothed densities, Bioinformatics, Vol. 20(5), pp. 623-628. 2004.

# ALU 2019 in matlab, reimplemented in R by MCT  07/2020
if(!requireNamespace("pracma")) stop("pracma package is missing")

if(missing(nbins)){ # default nbins in beiden Richtungen == 200
    nbins = c(min(length(unique(X)),2000) ,min(length(unique(Y)),2000) )
}else if(is.null(nbins)){
  nbins = c(min(length(unique(X)),2000) ,min(length(unique(Y)),2000) ) # default nbins in beiden Richtungen == 200
}else if(length(nbins)==1){  #nbins gilt fuer beide richtungen
  nbins = c(nbins,nbins)
}else{

}
if(missing(lambda)){
  lambda = 20
}else if(is.null(lambda)){  # default smoothing value
  lambda = 20
}else{

}

X = as.vector(X)#colvector(X)
Y = as.vector(Y) #colvector(Y)
OrigN = length(X)
# NaN handling
NoNaNInd <- which(is.finite(X )&is.finite(Y))
X  = X [NoNaNInd]
Y = Y[NoNaNInd]

n  = length(X)  # Anzahl Daten
ny = length(Y)  # Anzahl Daten in Y
if(n != ny)   stop(' SmoothedDensitiesXY: length(X) is not the same as length(Y)')

# Kernels bauen
if(missing(Xkernels)&missing(Ykernels)){
  minx = min(X)#nanmin(X,[],1)
  maxx = max(X)#nanmax(X,[],1)
  miny = min(Y)#nanmin(Y,[],1)
  maxy = max(Y)#nanmax(Y,[],1)
  edges1 = seq(from = minx, to = maxx,length.out =  nbins[1]+1)
  Xkernels = edges1[1:(length(edges1)-1)] + 0.5*diff(edges1)
  edges1 = c(-Inf, edges1[2:(length(edges1)-1)], Inf)
  edges2 =  seq(from = miny, to=maxy, length.out =  nbins[2]+1)
  Ykernels = edges2[1:(length(edges2)-1)] + 0.5*diff(edges2)
  edges2 = c(-Inf, edges2[2:(length(edges2)-1)], Inf)
}else if(missing(Xkernels)){
  stop('SmoothedDensitiesXY: Ykernels given but Xkernels missing, case not implemented!')
}else if(missing(Ykernels)){
  stop('SmoothedDensitiesXY: Xkernels given but Ykernels missing, case not implemented!')
}else{ # Xkernels,Ykernels gegeben
  minx = min(X)#nanmin(X,[],1)
  maxx = max(X)#nanmax(X,[],1)
  miny = min(Y)#nanmin(Y,[],1)
  maxy = max(Y)#nanmax(Y,[],1)
    edges1 = seq(from = minx, to = maxx,length.out =  nbins[1]+1)
	  edges1 = c(-Inf, edges1[2:(length(edges1)-1)], Inf)
    edges2 =  seq(from = miny, to=maxy, length.out =  nbins[2]+1)
    edges2 = c(-Inf, edges2[2:(length(edges2)-1)], Inf)
}  # if nargin < 5

# histogramme in X und Y bauen
bin = matrix(0,n,2)
# Reverse the columns to put the first column of X along the horizontal axis, the second along the vertical.
bin[,2] = pracma::histc(X,edges1)$bin   # histogram in X -> bin2
bin[,1] = pracma::histc(Y,edges2)$bin  # histogram in Y -> bin1

# smoothing der Histograme ergibt rohe Density F
H = pracma::accumarray(bin,rep(1,nrow(bin))  ,nbins[c(2,1)]) / n
#   accumarray(ind,val, sz)  value= 1  ind = bin   H  = 2D array  Resultat 2D Histogram

G = smooth1D(H,nbins[2]/lambda)    # in die eine Richtung smoothen
hist_F_2D = t(smooth1D(t(G),nbins[1]/lambda))  # F ist  das smoothed 2D histogram

# hist_F_2D prozentuieren, max Density == 1
MaxF = max(as.vector(hist_F_2D))
hist_F_2D = hist_F_2D /MaxF  # hist_F_2D prozentuieren, max Density == 1

# index fuer die Flaechen, passend zu X,Y,
m=dim(hist_F_2D)[1] #number of rows in the matrix
#matlab: ind = sub2ind(size(A), r, c)
#R: ind = (c-1)*m + r
#ind = sub2ind(size(hist_F_2D),bin(:,1),bin(:,2))  # index fuer die Flaechen, passend zu X,Y,
ind = (bin[,2]-1)*m + bin[,1]
XYDensity = hist_F_2D[ind]

# Density mit Nans
Density = matrix(0,OrigN,1)     # alles 0
Density[NoNaNInd] = XYDensity   # die berechnete dichte

Density=as.vector(Density)

# function Z = smooth1D(Y,lambda)
# 	[m,n] = size(Y)
# 	E = eye(m)
# 	D1 = diff(E,1)
# 	D2 = diff(D1,1)
# 	P = lambda.^2 .* D2'*D2 + 2.*lambda .* D1'*D1
# 	Z = (E + P) \ Y
# # end function Z = smooth1D(Y,lambda)
if(isTRUE(PlotIt)) {
  zplot(X,Y,Density)
}
return(list(Densities=as.vector(Density),Xkernels=Xkernels,Ykernels=Ykernels,hist_F_2D=hist_F_2D,ind=ind))
}
