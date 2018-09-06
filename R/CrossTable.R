CrossTablePlot=function(Data,xbins = seq(0, 100, 5),ybins= seq(0, 100, 5),PlotIt=T){
  
  aint=findInterval(Data[,1], ybins,left.open=F,rightmost.closed = F)
  bint=findInterval(Data[,2], ybins,left.open=F,rightmost.closed = F)
  
  u=unique(aint)
  leera=setdiff(1:length(xbins),u)
  
  u=unique(bint)
  leerb=setdiff(1:length(xbins),u)
  
  CrossTable=table(c(aint,leera,rep(1,length(leerb))),c(bint,rep(1,length(leera)),leerb))


  freq <-  as.data.frame(CrossTable)
  freq[,1] <- as.numeric(freq[,1])
  freq[,2] <- as.numeric(freq[,2])
  
  for(i in 1:length(leera)){
    freq[freq[,1]==leera[i],3]=0
  }
  
  for(i in 1:length(leerb)){
    freq[freq[,2]==leerb[i],3]=0
  }
  
  freq2D <- diag(21)*0
  freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]/60
  
  # Normal
  if(PlotIt){
    cols=DataVisualizations::HeatmapColors
    image(xbins, ybins, freq2D,xlab = colnames(Data)[1],ylab = colnames(Data)[2],col = cols)
    text(freq[,1]*5-5 ,freq[,2]*5-5 ,labels = round(freq[,3]/60,0))
  }
  return(invisible(CrossTable))
}