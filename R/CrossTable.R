CrossTablePlot=function(Data,xbins = seq(0, 100, 5),ybins= seq(0, 100, 5),PlotIt=T,NormalizationFactor=1){
  #interval is left closed and right opend
  width = abs(xbins[2] - xbins[1])
  aint = findInterval(Data[, 1],
                      ybins,
                      left.open = FALSE,
                      rightmost.closed = FALSE)
  bint = findInterval(Data[, 2],
                      ybins,
                      left.open = FALSE,
                      rightmost.closed = FALSE)
  #Crrection if a bin missing
  u = unique(aint)
  leera = setdiff(1:length(xbins), u)
  u = unique(bint)
  leerb = setdiff(1:length(xbins), u)
  #Fill up also mising bins
  CrossTable = table(c(aint, leera, rep(1, length(leerb))), c(bint, rep(1, length(leera)), leerb))
  #Transform
  frequency <-  as.data.frame(CrossTable)
  frequency[, 1] = as.numeric(frequency[, 1])
  frequency[, 2] = as.numeric(frequency[, 2])
  #Correction: set missing bins to zero
  for (i in 1:length(leera)) {
    frequency[frequency[, 1] == leera[i], 3] = 0
  }
  for (i in 1:length(leerb)) {
    frequency[frequency[, 2] == leerb[i], 3] = 0
  }
  #FillUp
  frequency2D = diag(length(xbins)) * 0
  frequency2D[cbind(frequency[, 1], frequency[, 2])] = frequency[, 3] /
    NormalizationFactor
  
  if (PlotIt) {
    cols = DataVisualizations::HeatmapColors
    image(
      xbins,
      ybins,
      frequency2D,
      xlab = colnames(Data)[1],
      ylab = colnames(Data)[2],
      col = cols
    )
    text(
      frequency[, 1] * width - width ,
      frequency[, 2] * width - width ,
      labels = round(frequency[, 3] / NormalizationFactor, 0)
    )
  }
  return(invisible(CrossTable / NormalizationFactor))
}