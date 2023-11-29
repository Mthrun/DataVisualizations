ShepardScatterPlot =Sheparddiagram=function(InputDists,OutputDists,xlab='Input Distances',ylab='Output Distances',fancy=F,main='ProjectionMethod',gPlot=ggplot()){
# Sheparddiagram(InputDists,OutputDists)
# Zeichnet ein Shepard Diagram
#
# INPUT
# InputDists             Matrize der Distanzen des Eingaberaumes
# OutputDists            Matrize der Distanzen des Ausgaberaumes 
# 
# Optional
# xlab,yxlabel        Achsenbeschriftung
# fancy                 =FALSE for PC, =TRUE for publication
# main                 title of shepard diagram
# gPlot                 objekt of ggplot 2, see doku there
# Author: MT 03/2014
# 1.Editor: MT 01/2016 umstieg auf ggplot2

  if(!is.matrix(InputDists)){
    warning('InputDists is not a matrix. Calling as.matrix()')
    InputDists=as.matrix(InputDists)
  }
  if(!is.matrix(OutputDists)){
    warning('OutputDists is not a matrix. Calling as.matrix()')
    OutputDists=as.matrix(OutputDists)
  }
  if(!mode(InputDists)=='numeric'){
    warning('InputDists is not a numeric matrix. Calling mode(InputDists)="numeric"')
    mode(InputDists)='numeric'
  }
  if(!mode(OutputDists)=='numeric'){
    warning('OutputDists is not a numeric matrix. Calling mode(OutputDists)="numeric"')
    mode(OutputDists)='numeric'
  }
  df = data.frame("InDist" = InputDists[lower.tri(InputDists, diag = FALSE)], "OutDist" = OutputDists[lower.tri(OutputDists, diag = FALSE)],main=factor(main))
  
  plt1 <- gPlot + geom_point(data = df, aes(x = .data$InDist, y = .data$OutDist)) +
    ylab(ylab)+xlab(xlab)+ggtitle(main)#+
    #geom_line(data = df, aes(x = InDist, y = InDist),color='red')
if(isTRUE(fancy)){
  plt1=plt1+
    theme(panel.background = element_blank(), legend.key = element_blank(),axis.line =element_line(colour='black'),
          axis.title.y = element_text(size = rel(2), angle = 90),
          axis.title.x = element_text(size = rel(2), angle = 00),
          axis.text.x = element_text(size = rel(2)),
          axis.text.y = element_text(size = rel(2)),
          plot.title =  element_text(size = rel(2))
    )+#coord_fixed(ratio=max(df$InDist)/max(df$OutDist))+
    coord_fixed(ratio=1)+
   # geom_line(data = df, aes(x = InDist, y = InDist),color='red',size=1.5)+
    coord_cartesian(xlim=range(df$InDist),ylim=range(df$OutDist),expand=FALSE)
}
  return(plt1)
}