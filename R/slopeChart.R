slopeChart=function(FirstDatavector,SecondDatavector,Names,Labels,MaxNumberOfSlices,TopLabels=c('FirstDatavector','SecondDatavector'),main='Comparision of Descending Frequency'){

  requireNamespace('plotrix')

  x1=internpiechart(FirstDatavector,Names,Labels,MaxNumberOfSlices=MaxNumberOfSlices)
  x2=internpiechart(SecondDatavector,Names,Labels,MaxNumberOfSlices = MaxNumberOfSlices)
  Percents1=x1$Percents
  Percents2=x2$Percents
  names1=names(Percents1)
  names2=names(Percents2)

  ind=match(table = names2,x = names1)
  n=length(Percents1)

  xy=cbind(Percents1,Percents2[ind])
  xy=xy[order(xy[,1],decreasing = T),]

  plotrix::bumpchart(xy,lwd = 3,names(x1$Percents),col=DataVisualizations::DefaultColorSequence[1:n],top.labels = NA,rank=T,arrows=F)
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]

  plotrix::boxed.labels(x = mean(c(xmax,xmin))-xmin/xmax,y = mean(c(ymax,ymin))+mean(c(ymax,ymin)),labels = TopLabels[1],border = F)
  plotrix::boxed.labels(x = mean(c(xmax,xmin))+xmin/xmax,y = 2*mean(c(ymax,ymin)),labels = TopLabels[2],border = F)
  plotrix::boxed.labels(x = mean(c(xmax,xmin)),y = ymin-0.4*ymin,labels = main,border = F,cex = 1.5)
  
}