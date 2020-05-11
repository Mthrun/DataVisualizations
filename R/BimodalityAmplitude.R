BimodalityAmplitude = function(x,PlotIt=FALSE){

  dens<-density(x)#crude but fast estimation
  dd<-data.frame(dens$x,dens$y)
  
  maxima.ind = which(diff(sign(diff(dd[,2])))==-2)+1
  if(length(maxima.ind)==0){#Wendepunkt nicht auffindbar
    return(0)
  }
  minima.ind_tmp = which(diff(sign(diff(dd[,2])))==2)+1
  if(length(minima.ind_tmp)==0){#Wendepunkt nicht auffindbar
    return(0)
  } 
  
  minima.ind=minima.ind_tmp[which(minima.ind_tmp>maxima.ind[1]&minima.ind_tmp<tail(maxima.ind,1))]
  
  maxima=matrix(NaN,nrow = length(minima.ind),ncol = 2)
  minima=rep(NaN,length(minima.ind))
  min.maxima=rep(NaN,length(minima.ind))
  antinode=rep(NaN,length(minima.ind))
  B=rep(NaN,length(minima.ind))
  for(i in 1:length(minima.ind)){
    min_ind=minima.ind[i]
    max1=maxima.ind[sort(which(maxima.ind<min_ind),decreasing = T)[1]] #hoehster kleiner als
    max2=maxima.ind[sort(which(maxima.ind>min_ind),decreasing = F)[1]] #hoehster er groeser ist

    maxima[i,] = dd[,1][c(max1,max2)]
    minima[i] = dd[,1][min_ind]
    
    min.maxima[i] = min(dd[,2][c(max1,max2)])
    antinode[i] = dd[,2][min_ind]
    
    B[i]=(min.maxima[i]-antinode[i])/min.maxima[i]
  }
  select=which.max(B)
  B=B[select]
  maxima=maxima[select,]
  minima=minima[select]
  
  if (isTRUE(PlotIt)){
    mm=min(dd[,1])
    if(abs(mm)<1e-2) mm=-0.02
    
    xlim=c(mm,max(dd[,1])*1.02)
    plot(dd[,1],dd[,2],type='l', main="Pareto Density plot with Peaks and Antimodes 'A'",ylim =c(0, max(dd[,2])*1.1),xlim=xlim,xlab='Kernels',ylab='Density')
    abline(v=maxima[1],col="blue", lwd=2)
    abline(v=maxima[2],col="blue", lwd=2)
    abline(v=minima,col="darkgreen", lwd=2)
    text((max(maxima)+.06*diff(range(dd[,1]))),1.009*max(range(dd[,2])), "Max 1", col = "red") 
    text((min(maxima)-.06*diff(range(dd[,1]))),1.009*max(range(dd[,2])), "Max 2", col = "red") 
    text((minima+.06*diff(range(dd[,1]))),0, "A", col = "red") 
  }
  
  return(B)	
}