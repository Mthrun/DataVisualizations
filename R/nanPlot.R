nanPlot=function(Data,Names){
# nanPlot(Data) 
# nanPlot(Data,Names)
# Plotet als Balkendiagram den prozetualen anteil von NaN je Variable
# Input
# Data[1:n,1:d] 		matrix aus Daten
# Optional	
# Names[1:D]				Vector aus Strings, s. ReadLRN(), ReadData()
#author: MT 07/2015
AlleNans=c()
if(!is.matrix(Data)){
  warning('Data is not a matrix. Calling as.matrix()')
  Data=as.matrix(Data)

} 
AnzVariablen=ncol(Data)

if(missing(Names)){
  Names=c()
  for(i in 1:AnzVariablen)
    Names[i]=paste0('Col ',i)
}
for (i in 1:AnzVariablen)  {
  AlleNans[i]=sum(!is.finite(Data[,i]))
} # for (i=1:AnzVariablen)   
ListeV=list('topright',border='white',cex=0.9,bty = "n")
Percentual=AlleNans/nrow(Data)*100
barplot(Percentual,horiz=TRUE,names.arg=Names[1:AnzVariablen],
legend.text=Names[1:AnzVariablen],args.legend=ListeV,cex.names=0.7,space=0.5,
border=NA,xlim=c(0,100),col=c('red','blue','green','black','grey'),xlab='NaN in percent %',main='Amount of missing data')
AllNaN=cbind(AlleNans,Percentual)
rownames(AllNaN)=Names

return(invisible(AllNaN))
}