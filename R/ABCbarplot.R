ABCbarplot=ABC_screeplot=function(Data,Colors=DataVisualizations::DefaultColorSequence[1:3],main,xlab="Fraction of Data",ylab="Value"){
# V= ABCbarPlot(Data);
# V has the elements Aind,Bind,Cind,ABlimit,BClimit
# barplot of sortdescending(Data) with the sets in different colours
# plot also : two diagonals and  ABC curve for Uniform distibution U[0,max(data) and   Horizintal and Vertical limits for setsABC 
# 
# INPUT
# Data(1:n)         data points, for ABCplots the  CleanedData = ABCcleanData(Data) are used
                                                                       
# OPTIONAL
# Colors             colors for A,B,C ; default:  DefaultColorSequence
if(missing(main)) main="Barplot colored by ABCanalysis Indicating Most-Important Values"
Data=Data[is.finite(Data)]
SortedData = sort(Data,decreasing = T,na.last = T)
V = ABCanalysis::ABCanalysis(SortedData)
Aind =V$Aind
Bind =V$Bind
Cind =V$Cind

X = c(1:length(SortedData))
ColorsVec=c(rep(Colors[1],length(Aind)),rep(Colors[2],length(Bind)),rep(Colors[3],length(Cind)))
ABC_classes=c(rep('A',length(Aind)),rep('B',length(Bind)),rep('C',length(Cind)))
DF=data.frame(X=X/100,Y=SortedData,ABC_classes=ABC_classes,ColorsVec=ColorsVec)

ggobject=ggplot(data=DF, aes(x=X,y = Y,fill=ABC_classes)) + geom_bar(stat="identity")+ 
  scale_fill_manual(values = Colors, name = "ABC Analysis")+
  ggtitle(main)+xlab(xlab)+ylab(ylab)
print(ggobject)
return(list(ABCanalysis=V,ggobject=ggobject,DF=DF))
}