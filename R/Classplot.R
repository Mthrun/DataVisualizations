Classplot = function(X, Y, Cls,
                     Plotter,
                     Names = NULL,
                     na.rm = FALSE,
                     xlab = "X",
                     ylab = "Y",
                     main = "Class Plot",
                     Colors = NULL,
                     Size = 8,
                     LineColor = NULL,
                     LineWidth = 1,
                     LineType  = NULL,
                     Showgrid  = TRUE,
                     pch = 20, 
                     AnnotateIt = FALSE,
                     SaveIt = FALSE, 
                     Nudge_x_Names = 0,
                     Nudge_y_Names = 0,
                     Legend = "",
                     ...){
  
  if(missing(Cls)) Cls=rep(1,length(X))
  if(missing(xlab)) xlab=deparse1(substitute(X))
  if(missing(ylab)) ylab=deparse1(substitute(Y))
 
  X=checkFeature(X,varname='X',Funname="Classplot")
  Y=checkFeature(Y,varname='Y',Funname="Classplot")
  Cls=checkCls(Cls,length(Y))
  
  
  if(length(X)!=length(Y)) stop('X and Y have to have the same length')
  
  if(isTRUE(na.rm)){ #achtung irgendwas stimmt hier nicht
    noNaNInd <- which(is.finite(X)&is.finite(Y))
    X = X[noNaNInd]
    Y = Y[noNaNInd]
    Cls=Cls[noNaNInd]
    
    if(!is.null(Names)){
      Names=Names[noNaNInd]
    }
  }
  u=unique(Cls)
  uu=sort(u,decreasing = F)
  
  if(!is.null(Names)){
    #for legend
    u_names=unique(Names)[order(u,decreasing = F)]
  }else{
    u_names=as.character(uu)
  }
  if(is.null(Colors)){
    mc=length(uu)
    if(is.null(Names))
    Colors=DataVisualizations::DefaultColorSequence[1:mc]
    else
      Colors=DataVisualizations::DefaultColorSequence[-2][1:mc] #no yellow
  }
  
  ColorVec=Cls*0
  k=1
  for(i in uu){
    ColorVec[Cls==i]=Colors[k]
    k=k+1
  }
  
  if(missing(Plotter)){
    if(is.null(Names)){
      Plotter="plotly"
    }else{
      Plotter="ggplot"
    }
  }
  if(Plotter=="ggplot2") Plotter="ggplot"
  
  
  if(Plotter=="plotly"){
    if(!requireNamespace('plotly',quietly = TRUE)){
    
      message('Subordinate package (plotly) is missing. No computations are performed.
              Please install the package which is defined in "Suggests".')
            
      return('Subordinate package (plotly) is missing. No computations are performed.
             Please install the package which is defined in "Suggests".')
  }
  p <- plotly::plot_ly()
  
  if(!is.null(LineColor)){
    p <- plotly::add_lines(p, x = ~X, y = ~Y,
                           line = list(color = LineColor,
                                       width = LineWidth,
                                       dash  = LineType),
                           name = 'Line')
  }
   
  if(!is.null(Names)){
    UniqueNames = unique(Names)
    for(i in 1:length(UniqueNames)){
      DataIdx = which(Names == UniqueNames[i])
      p = plotly::add_markers(p = p,
                              x = X[DataIdx],
                              y = Y[DataIdx],
                              type = "scatter",
                              mode = "marker",
                              name = UniqueNames[i],
                              marker = list(size = Size,
                                            color = Colors[i],
                                            line = list(color = "black",
                                                        width = 1)))
    }
  }else{
    p = plotly::add_markers(p = p,
                            x = X,
                            y = Y,
                            type = "scatter",
                            mode = "marker",
                            marker = list(size = Size,
                                          color = Colors[Cls],
                                          line = list(color = "black",
                                                      width = 1)))
  }
  
  p <- plotly::layout(p,
                      legend = list(title = list(text = Legend)),
                      title = main,
                      margin = list(l = 20, r = 0, b = 0, t = 70, pad = 10),
                      xaxis = list(title     = xlab,
                                   showgrid  = Showgrid,
                                   linewidth = 1,
                                   zeroline  = FALSE,
                                   mirror    = TRUE), 
                      yaxis = list(title     = ylab,
                                   showgrid  = Showgrid,
                                   linewidth = 1,
                                   zeroline  = FALSE,
                                   mirror    = TRUE))

  p

    if(isTRUE(SaveIt)){
      requireNamespace("htmlwidgets")
      htmlwidgets::saveWidget(p, file = "Classplot.html")
    }
    return(p)
  }
  
  if(Plotter=="ggplot"){
    
    df=data.frame(X=X,Y=Y,Cls=Cls)
    if(!is.null(Names))
      df$Names = rownames(Names)

    if(is.null(Names)){
      df$Names = Cls
    }
   #    

    df$Colors=ColorVec
    
    #colMat <- grDevices::col2rgb(Colors)
    #hex=grDevices::rgb(red = colMat[1, ]/255, green = colMat[2, ]/255, blue = colMat[3,]/255)
    
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "X",
                                                 y = "Y",
                                                 label = "Names",
                                                 group="Cls",
                                                 color="Colors"),...)+
      ggplot2::theme_bw()
    
    if(missing(pch)){ #black shape around circular points
      p=p+ggplot2::geom_point(size = Size)+ggplot2::geom_point(size = Size,pch=21, colour="Black",alpha=0.4)
    }else{#points have various shapes
      p=p+ggplot2::geom_point(size = Size,shape=pch)
    }
    
    if(Legend != ""){
      #overlay the with specific colors filled out points with black points that are empty inside
      #=> points now get a black border
      p = p + ggplot2::scale_color_identity(name=Legend,breaks=Colors,labels=u_names,guide="legend")
    }else{
      p = p + ggplot2::scale_color_identity()
    }
    if(!is.null(LineType))
      p = p + geom_line(aes_string(group = "Cls"))
    
    if(!is.null(Names) & (AnnotateIt == TRUE))
      p <- p + ggrepel::geom_text_repel(nudge_y=Nudge_y_Names,nudge_x=Nudge_x_Names) 

    
    p = p + ggplot2::ggtitle(label =  main) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      theme(plot.title = element_text(hjust = 0.5))
    p
    
    
    if (isTRUE(SaveIt)) {
      ggplot2::ggsave(filename ="Classplot.png" ,plot=p,device = "png")
   
    }
    
    return(p)
  }
  if(Plotter!="native")
    message('Incorrect plotter selected, performing simple native plot')
  
  if(Size==8){
    Size=Size-6 #adapting default size
  }
  
  plot(X,Y,col=ColorVec,main=main,xlab=xlab,ylab = ylab,type='p',cex=Size,pch=pch,...)
  if(!missing(Legend)){
    legend("topright",title=Legend,legend=unique(Cls),col=unique(ColorVec),pch=unique(pch),box.lty=0)
  }
}

