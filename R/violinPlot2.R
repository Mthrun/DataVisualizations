violinPlot2=function(ListOfKernels,ListOfDensities){
 KernelCurrent= ListOfKernels[[1]]
 CurrentDensity=ListOfDensities[[1]]
 displayn=length(ListOfDensities)
 #at <- 1:displayn
 at=1
 #wd <- maxwidth/max(unlist(dListOfDensities))
 wd=2885
 boxwex=1
 wd2 <- wd * boxwex/2
 
 #at <- 1:displayn
 x1=CurrentDensity*wd2+at
 y1=KernelCurrent
 x2=rev(CurrentDensity)*-wd2+at
 y2=rev(KernelCurrent)
 # x1 <- dens[["y", i]] * wd2 + at[i]
 # y1 <- dens[["x", i]]
 # 
 # x2 <- rev(dens[["y", i]]) * -wd2 + at[i]
 # y2 <- rev(dens[["x", i]])
 
 # x1 <- c(at[i], x1, at[i])
 # y1 <- c(y1[1], y1, y1[length(y1)])
 # 
 # x2 <- c(at[i], x2, at[i])
 # y2 <- c(y2[1], y2, y2[length(y2)])
 plot(c(min(y1), max(y2)), c(min(x1), max(x2)))
 polygon(c(y1, y2), c(x1, x2), col = 'black', border = NULL)
}