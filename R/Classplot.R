Classplot = function(X, Y, Cls, Plotter, Names = NULL, Subsample = TRUE, 
                     na.rm = FALSE, xlab = "X", ylab = "Y", main = "Class Plot",
                     Colors = NULL, Size = 8, PointBorderCol, LineColor = NULL, 
                     LineWidth = 1, LineType = NULL, Showgrid = TRUE, pch, AnnotateIt = FALSE, 
                     SaveIt = FALSE, Nudge_x_Names = 0, Nudge_y_Names = 0, Legend = "",
                     SmallClassesOnTop = TRUE, ...){
  
  if(missing(Cls)) Cls=rep(1,length(X))
  if(missing(xlab)) xlab=deparse1(substitute(X))
  if(missing(ylab)) ylab=deparse1(substitute(Y))
  
  if(missing(PointBorderCol)&&missing(pch)){
    PointBorderRequested=TRUE
    PointBorderCol="black"
  }else if(missing(PointBorderCol)&&!missing(pch)&&all(pch %in% 21:25)){
    PointBorderRequested=TRUE
    PointBorderCol="black"
  }else if(missing(PointBorderCol)&&!missing(pch)&&any(!pch %in% 21:25)){
    PointBorderRequested=FALSE
  }else if(!missing(PointBorderCol)){
    if(is.null(PointBorderCol)){
      PointBorderRequested=FALSE
    }else if(length(PointBorderCol)!=1){
        warning("PointBorderCol must contain exactly one color. Ignoring input.")
        PointBorderRequested=FALSE
    }else if(isFALSE(PointBorderCol)){
      PointBorderRequested=FALSE
    }else if(is.na(PointBorderCol)){
      PointBorderRequested=FALSE
    }else{
      PointBorderRequested=TRUE
    }
  }else{
    PointBorderRequested=FALSE
  }
  
  # Keep the attached validation helpers, but validate X/Y length and empty input before adapting Cls.
  X=checkFeature(X,varname="X",Funname="Classplot")
  Y=checkFeature(Y,varname="Y",Funname="Classplot")
  if(length(X)!=length(Y)) stop("X and Y have to have the same length")
  if(length(X)==0) stop("X and Y must contain at least one value")
  Cls=checkCls(Cls,length(Y),Normalize=FALSE)
  
  # Validate scalar numeric controls before they are used in plotting calls or if checks.
  if(length(Size)!=1||!is.numeric(Size)||!is.finite(Size)||Size<=0) stop("Size must be one finite numeric value greater than zero")
  if(length(LineWidth)!=1||!is.numeric(LineWidth)||!is.finite(LineWidth)||LineWidth<0) stop("LineWidth must be one finite non-negative numeric value")
  
  if(!missing(pch)){
    if(length(pch)==1){
      pch=rep(pch,length(X))
    }else if(length(X)!=length(pch)){
      pch=rep(20,length(X))
      warning("X and pch have to have the same length. Setting pch=20")
    }
  }
  
  # Names is one value per observation; shorten excess values, but reject too few values instead of silently padding with NA.
  if(!is.null(Names)){
    Names=as.character(Names)
    if(length(Names)<length(X)) stop("Names must contain one value for every observation in X")
    if(length(Names)>length(X)){
      Names=Names[seq_len(length(X))]
      warning("Names was longer than X and was shortened to the length of X")
    }
  }
  
  # Make Legend checks safe for NULL, NA, non-character, or non-scalar values.
  ShowLegend=is.character(Legend)&&length(Legend)==1&&!is.na(Legend)&&nzchar(Legend)
  
  #  Remove non-finite X, Y, and Cls together; otherwise stop before plotting or subsampling can misalign observations.
  noNaNInd=which(is.finite(X)&is.finite(Y)&is.finite(Cls))
  if(isTRUE(na.rm)){
    X=X[noNaNInd]
    Y=Y[noNaNInd]
    Cls=Cls[noNaNInd]
    if(!is.null(Names)){
      Names=Names[noNaNInd]
    }
    if(!missing(pch)){
      pch=pch[noNaNInd]
    }
  }else if(length(noNaNInd)!=length(X)){
    stop("X, Y, and Cls must contain only finite values when na.rm=FALSE")
  }
  if(length(X)==0) stop("No finite observations remain after removing non-finite values")
  
  # Check ScatterDensity explicitly and keep SampleScatter indices aligned by disabling its internal NA filtering.
  if(isTRUE(Subsample)){
    if(length(X)>5000){
      if(!requireNamespace("ScatterDensity",quietly=TRUE)){
        warning("Subordinate package ScatterDensity is required when Subsample=TRUE and more than 5000 observations are supplied. Fallback to uniform sample.")
        indsub=sample(1:length(X),5000)
      }else{
        indsub=ScatterDensity::SampleScatter(X,Y,na.rm=FALSE,PlotIt=FALSE)
      } 
      if(length(indsub)==0||any(!is.finite(indsub))||any(indsub<1)||any(indsub>length(X))) stop("SampleScatter returned invalid observation indices")
    }else{
      Subsample=FALSE
    }
  }
  if(isTRUE(Subsample)){
    X=X[indsub]
    Y=Y[indsub]
    Cls=Cls[indsub]
    
    if(!is.null(Names)){
      Names=Names[indsub]
    }
    if(!missing(pch)){
      pch=pch[indsub]
    }
  }
  
  # Preserve observation order for the Plotly line before marker-order changes are applied.
  LineX=X
  LineY=Y
  
  # Use sorted class labels for deterministic color/name mapping and derive one legend name from each class.
  uniqueLabels=sort(unique(Cls),decreasing=FALSE)
  uu=uniqueLabels
  if(!is.null(Names)){
    u_names=character(length(uu))
    for(i in seq_along(uu)){
      CurrentClass=uu[i]
      CurrentNames=unique(Names[Cls==CurrentClass])
      CurrentNames=CurrentNames[!is.na(CurrentNames)]
      if(length(CurrentNames)==0){
        u_names[i]=as.character(CurrentClass)
      }else{
        u_names[i]=as.character(CurrentNames[1])
      }
    }
  }else{
    u_names=as.character(uu)
  }  
  # Restrict Colors to exactly one valid color per present class, support extra named colors safely, and reject too few colors.
  mc=length(uu)
  if(is.null(Colors)){
    if(is.null(Names)) 
		DefaultColors=DataVisualizations::DefaultColorSequence 
    else 
		DefaultColors=DataVisualizations::DefaultColorSequence[-2]
    if(length(DefaultColors)<mc) 
      stop("The default color sequence is shorter than the number of classes; supply Colors explicitly")
    Colors=DefaultColors[seq_len(mc)]
  }else{
    ColorNames=names(Colors)
    Colors=as.character(Colors)
    names(Colors)=ColorNames
    if(is.null(names(Colors))){
      if(length(Colors)<mc) stop("Colors must contain at least one color for every class")
      Colors=unname(Colors[seq_len(mc)])
    }else{
      class_color=suppressWarnings(as.numeric(names(Colors)))
      if(any(!is.finite(class_color))||!all(uu%in%class_color)){
        warning("Classplot: Names of Colors do not contain all finite numeric labels of Cls. Falling back to positional color mapping.")
        if(length(Colors)<mc) stop("Colors must contain at least one color for every class")
        Colors=unname(Colors[seq_len(mc)])
      }else{
        Colors=unname(Colors[match(uu,class_color)])
      }
    }
  }
  tryCatch(grDevices::col2rgb(Colors),error=function(e) stop("Colors contains at least one invalid R color value"))
  
  ## Make sure that small classes are plotted last and remain visible.
  cp=table(Cls)
  if(isTRUE(SmallClassesOnTop)) 
    indBig2Small=order(cp,decreasing=TRUE) 
  else 
	indBig2Small=seq_along(cp)
  
  # Reorder markers only when requested, keep class names/colors synchronized, and use seq_along for zero-safe iteration.
  if(isTRUE(SmallClassesOnTop)){
    uug=as.numeric(names(cp))
    Colors=Colors[indBig2Small]
    u_names=u_names[indBig2Small]
    if(!is.null(Names)) 
      NamesOrdered=character(0)
    ClsOrdered=numeric(0)
    Xordered=numeric(0)
    Yordered=numeric(0)
    if(!missing(pch)) pchordered=numeric(0)
    for(k in seq_along(cp)){
      ind_o=which(Cls==uug[indBig2Small[k]])
      ClsOrdered=c(ClsOrdered,Cls[ind_o])
      Yordered=c(Yordered,Y[ind_o])
      Xordered=c(Xordered,X[ind_o])
      if(!is.null(Names)) 
        NamesOrdered=c(NamesOrdered,Names[ind_o])
      if(!missing(pch)) 
        pchordered=c(pchordered,pch[ind_o])
    }
    X=Xordered
    Y=Yordered
    Cls=ClsOrdered
    if(!is.null(Names)) 
      Names=NamesOrdered
    if(!missing(pch)) 
      pch=pchordered
    uniqueLabels=unique(Cls)
  }
  
  # Build colors from the already validated class-color order; remove the invalid named-color loop and NA assignments.
  ColorVec=rep(NA_character_,length(Cls))
  for(k in seq_along(uniqueLabels)) 
    ColorVec[Cls==uniqueLabels[k]]=Colors[k]
  
  if(missing(Plotter)){
    if(is.null(Names)) Plotter="plotly" else Plotter="ggplot"
  }else{
    if(length(Plotter)!=1||is.na(Plotter)) stop("Plotter must be one non-missing value")
    Plotter=as.character(Plotter)
  }
  if(Plotter=="ggplot2") Plotter="ggplot"
  
  if(Plotter=="plotly"){
    if(!requireNamespace("plotly",quietly=TRUE)){
      message("Subordinate package plotly is missing. No computations are performed. Please install the package defined in Suggests.")
      return("Subordinate package plotly is missing. No computations are performed. Please install the package defined in Suggests.")
    }
    
    # Forward ... to plotly::plot_ly and use only = assignments.
    p=plotly::plot_ly(...)
    
    if(isFALSE(PointBorderRequested)){
      borderWidth=0
    }else{
      borderWidth=1
      if(Size<=1) 
        borderWidth=0 
      else if(Size<=2) 
        borderWidth=0.2 
      else if(Size<=3) 
        borderWidth=0.7
    }
    
    # Draw Plotly lines in original/subsampled observation order rather than class-reordered marker order.
    if (!is.null(LineColor))
      p = plotly::add_lines(
        p,
        x = LineX,
        y = LineY,
        line = list(
          color = LineColor,
          width = LineWidth,
          dash = LineType
        ),
        name = "Line"
      )
    
    # Remove the unreachable Colors=NULL branch, use valid mode="markers", add class trace names, and expose point Names as hover text.
    for (i in seq_along(uniqueLabels)) {
      DataIdx = which(Cls == uniqueLabels[i])
      if (is.null(Names))
        HoverText=NULL
      else
        HoverText=Names[DataIdx]
      if (is.null(Names))
        HoverInfo = "x+y+name"
      else
        HoverInfo ="text+x+y+name"
      
      if(PointBorderRequested){
        p = plotly::add_markers(
          p = p,
          x = X[DataIdx],
          y = Y[DataIdx],
          type = "scatter",
          mode = "markers",
          name = u_names[i],
          text = HoverText,
          hoverinfo = HoverInfo,
          marker = list(
            size = Size,
            color = unique(ColorVec[DataIdx]),
            line = list(color = PointBorderCol, width = borderWidth)
          )
        )
      }else{
        p = plotly::add_markers(
          p = p,
          x = X[DataIdx],
          y = Y[DataIdx],
          type = "scatter",
          mode = "markers",
          name = u_names[i],
          text = HoverText,
          hoverinfo = HoverInfo,
          marker = list(
            size = Size,
            color = unique(ColorVec[DataIdx]),
            line = list(width = borderWidth)
          )
        )
      }

    }
    
    # Use the safe scalar ShowLegend check and keep each layout call on one line.
    if (ShowLegend)
      p = plotly::layout(
        p,
        legend = list(title = list(text = Legend)),
        title = main,
        margin = list(
          l = 20,
          r = 0,
          b = 0,
          t = 70,
          pad = 10
        ),
        xaxis = list(
          title = xlab,
          showgrid = Showgrid,
          linewidth = 1,
          zeroline = FALSE,
          mirror = TRUE
        ),
        yaxis = list(
          title = ylab,
          showgrid = Showgrid,
          linewidth = 1,
          zeroline = FALSE,
          mirror = TRUE
        )
      )
    else
      p = plotly::layout(
        p,
        title = main,
        showlegend = FALSE,
        margin = list(
          l = 20,
          r = 0,
          b = 0,
          t = 70,
          pad = 10
        ),
        xaxis = list(
          title = xlab,
          showgrid = Showgrid,
          linewidth = 1,
          zeroline = FALSE,
          mirror = TRUE
        ),
        yaxis = list(
          title = ylab,
          showgrid = Showgrid,
          linewidth = 1,
          zeroline = FALSE,
          mirror = TRUE
        )
      )
    
    # Check htmlwidgets before saving instead of ignoring requireNamespace's result.
    if(isTRUE(SaveIt)){
      if(!requireNamespace("htmlwidgets",quietly=TRUE)){
        warning("Subordinate package htmlwidgets is required when SaveIt=TRUE for Plotly output")
      }else{
        htmlwidgets::saveWidget(p,file="Classplot.html")
      } 
    }
    return(p)
  }
  
  if(Plotter=="ggplot"){
    # Check ggplot2 explicitly, use the Names vector itself, and reference all aesthetics through .data.
    if(!requireNamespace("ggplot2",quietly=TRUE)) 
      stop("Subordinate package ggplot2 is required for Plotter=ggplot")
    df=data.frame(X=X,Y=Y,Cls=Cls)
    if(!is.null(Names)) 
      df$Names=Names 
    else 
      df$Names=as.character(Cls)
    df$Colors=ColorVec
    if(!missing(pch)) 
      df$Shape=pch
    p = ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = .data$X,
        y = .data$Y,
        label = .data$Names,
        group = .data$Cls,
        color = .data$Colors
      )
    ) + ggplot2::theme_bw()
    
    UseFillScale=missing(pch)||all(pch%in%21:25)
    if(PointBorderRequested&&!missing(pch)&&any(!(pch %in% 21:25))){
      warning("PointBorderCol can only define a separate ggplot border for pch values 21 to 25. PointBorderCol is ignored for this pch vector.")
      PointBorderRequested=FALSE
      UseFillScale=FALSE
    }
    if(PointBorderRequested){
      if(missing(pch)){
        p=p+ggplot2::geom_point(ggplot2::aes(fill=.data$Colors),size=Size,shape=21,colour=PointBorderCol,...)
      }else{
        p=p+ggplot2::geom_point(ggplot2::aes(fill=.data$Colors,shape=.data$Shape),size=Size,colour=PointBorderCol,...)
      }
    }else{
      if(missing(pch)){
        p=p+ggplot2::geom_point(ggplot2::aes(fill=.data$Colors),size=Size,shape=21,...)
      }else{
        p=p+ggplot2::geom_point(ggplot2::aes(fill=.data$Colors,shape=.data$Shape),size=Size,...)
      }
    }
    if(!missing(pch)) 
      p=p+ggplot2::scale_shape_identity(guide="none")
    
    # Keep legend breaks and labels synchronized and use fill legends for shapes that support independent borders.
    if(UseFillScale){
      if(ShowLegend){
        p=p+ggplot2::scale_fill_identity(name=Legend,breaks=Colors,labels=u_names,guide="legend")+ggplot2::scale_color_identity()
      }else{
        p=p+ggplot2::scale_fill_identity()+ggplot2::scale_color_identity()
      }
    }else{
      if(ShowLegend){
        p=p+ggplot2::scale_color_identity(name=Legend,breaks=Colors,labels=u_names,guide="legend")+ggplot2::scale_fill_identity()
      }else{
        p=p+ggplot2::scale_color_identity()+ggplot2::scale_fill_identity()
      }
    }
    
    # Namespace ggplot functions and keep class-colored lines in class order without changing Plotly line order.
    if(!is.null(LineType)) p=p+ggplot2::geom_line(show.legend=FALSE)
    
    # Check ggrepel before annotation and use isTRUE for a safe scalar condition.
    if(!is.null(Names)&&isTRUE(AnnotateIt)){
      if(!requireNamespace("ggrepel",quietly=TRUE)){
        warning("Subordinate package ggrepel is required when AnnotateIt=TRUE")
      }else{
        p=p+ggrepel::geom_text_repel(nudge_y=Nudge_y_Names,nudge_x=Nudge_x_Names,show.legend=FALSE)
      } 
    }
    
    #  Namespace theme and element_text to make the function safe when ggplot2 is not attached.
    p = p + ggplot2::ggtitle(label = main) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    if(isTRUE(SaveIt)) 
      ggplot2::ggsave(filename="Classplot.png",plot=p,device="png")
    return(p)
  }
  
  if(Plotter!="native") 
    message("Incorrect plotter selected, performing simple native plot")
  if(Size==8) 
    Size=Size-6
  if(missing(pch)) 
    pch=20
  
  graphics::plot(X,Y,col=ColorVec,main=main,xlab=xlab,ylab=ylab,type="p",cex=Size,pch=pch,...)
  
  # Use the safe Legend flag and map native legend colors/shapes directly by class instead of relying on unrelated unique() orders.
  if(ShowLegend){
    if(length(pch)==1)
      LegendPch=rep(pch, length(uniqueLabels))
    else
      LegendPch=as.numeric(vapply(uniqueLabels, function(CurrentClass)
        pch[which(Cls == CurrentClass)[1]], numeric(1)))
    graphics::legend("topright",title=Legend,legend=u_names,col=Colors,pch=LegendPch,box.lty=0)
  }
}

