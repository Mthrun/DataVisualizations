ShepardDensityPlot=function(InputDists,OutputDists,Plotter='native',xlab='Input Distances',ylab='Output Distances',main='ProjectionMethod', sampleSize=50000){
  #  Plotter                 Plotting Backend to use. 
  #                          Possible values are: native, ggplot, plotly
  #
  #author: MT, 2017
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
  
  
   x=InputDists[lower.tri(InputDists, diag = FALSE)]
  y=OutputDists[lower.tri(OutputDists, diag = FALSE)]
  xn=length(x)
  yn=length(y)

  if(xn!=yn) stop('Number of distances is not equal')
  
 
  if(xn>sampleSize){
    ind=sample(size = sampleSize,replace = F,x = 1:xn)
    print('Too many distances. Drawing sample.')
		plot(x[ind],y[ind],xlab='Input',ylab='Output',main='shepard diagram')
    ggobject=PDEscatter(x[ind],y[ind],Plotter=Plotter,xlab=xlab, ylab=ylab, main=main)$Handle
  }else{
    ggobject=PDEscatter(x,y,Plotter=Plotter,xlab=xlab, ylab=ylab, main=main)$Handle
  }
  # ggobject=PDEscatter(x,y)
  # ggobject=ggobject+ylab('Output Distances')+xlab('Input Distances')+ggtitle(label)#+
  # if(fancy){
  #   ggobject=ggobject+
  #     theme(panel.background = element_blank(), legend.key = element_blank(),axis.line =element_line(colour='black'),
  #           axis.title.y = element_text(size = rel(2), angle = 90),
  #           axis.title.x = element_text(size = rel(2), angle = 00),
  #           axis.text.x = element_text(size = rel(2)),
  #           axis.text.y = element_text(size = rel(2)),
  #           plot.title =  element_text(size = rel(2))
  #     )+#coord_fixed(ratio=max(df$InDist)/max(df$OutDist))+
  #     coord_fixed(ratio=1)+
  #     # geom_line(data = df, aes(x = InDist, y = InDist),color='red',size=1.5)+
  #     coord_cartesian(expand=FALSE)
  # }
  return(ggobject)
}