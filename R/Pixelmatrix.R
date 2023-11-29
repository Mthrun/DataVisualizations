Pixelmatrix=PlotPixMatrix =  function(Data, XNames, LowLim, HiLim, YNames, main,FillNotFiniteWithHighestValue=FALSE) {
    #PixelMatrixPlot = function(Data, XNames=NULL, LowLim=NULL, HiLim=NULL, YNames=NULL,main='')
    # PixelMatrixPlot(Data,XNames,LowLim,HiLim,YNames);
    #  plot Data matrix as a pixel colour picture
    #
    #  INPUT
    #  Data[1:n,1:d]          Data cases in rows, variables in columns
    #
    #  OPTIONAL
    #  XNames[1:d,:]           names of the Data added at x/y -axis , XNames == NULL means no names
    
    
    #  Nota: Die 4 Verschiebungen muesen fuer einen gut ausehenden Plot gegebenenfalls empirisch angepasst werden
    #  author: Michael Thrun, Felix Pape                                                                                                                                       ?
    
    ### Define the Heat Color
  if(missing(main)) main=paste0("Dataset: ",deparse1(substitute(Data)))
  if(!is.matrix(Data)){
    warning('Data is not a matrix. Calling as.matrix()')
    Data=as.matrix(Data)
  }
  if(!mode(Data)=='numeric'){
    warning('Data is not a numeric matrix. Calling mode(Data)="numeric"')
    mode(Data)='numeric'
  }
  if(missing(XNames)){
    XNames=colnames(Data)
  }
  if(missing(YNames)){
    YNames=rownames(Data)
    if(is.null(YNames))
      YNames=1:nrow(Data)
  }
  heatC = DataVisualizations::HeatmapColors
  Schalter=TRUE
  if (missing(LowLim)&missing(HiLim))
    Schalter=FALSE
  
  if (missing(LowLim))
      LowLim = min(Data, na.rm = T)
  if (missing(HiLim))
      HiLim = max(Data, na.rm = T)
    
  isnumber=function(x) return(is.numeric(x)&length(x)==1)  
  if(!isnumber(HiLim))
    stop('"HiLim" is not a numeric number of length 1. Please change Input.')
  
  if(!isnumber(LowLim))
    stop('"LowLim" is not a numeric number of length 1. Please change Input.')
  
  if(HiLim <= LowLim){
      warning("HiLim should be bigger than LowLim, setting HiLim = LowLim + 0.1")
      HiLim = LowLim + 0.1
  }
    
    if (is.vector(Data)) {
      Data <- as.matrix(Data)
    }
    if (min(dim(Data)) < 1) {
      warning('PlotPixMatrix: Datamust be matrix with min 2 columns and 2 rows!')
    }
    if(isTRUE(Schalter)){
      hiind = which(Data >= HiLim)
      Data[hiind] = HiLim
      lowind = which(Data <= LowLim)
      Data[lowind] = LowLim
    }
    if(isTRUE(FillNotFiniteWithHighestValue)){
      if(any(!is.finite(Data))){ # vom Prinzip aus Matlab uebernommen
        Delta = (HiLim - LowLim) / 64
        Value4NaN = HiLim + Delta
        Data[which(Data == HiLim)] = HiLim - Delta
        Data[which(!is.finite(Data))] = Value4NaN
      }
    }
    df <- data.frame(Data)
    if (!is.null(XNames)) {
      names(df) <- XNames
    }
    
    df$id <- seq.int(nrow(df))
    dfm <- reshape2::melt(df, id = 'id')
    
    ## Variablen koennen sehr unterschiedliche Ranges haben,
    ## was zu einer schlechten Heatmap fuehren kann.
    ## darum transformieren wir jetzt die Daten zunaechst
    
    #dfm <- ddply(dfm, .(variable), transform,
    #             rescale = rescale(value))
    #aes only works if you dont modify your features in ggplot2 (e.g. not logarithmize them)
    plt <-
      ggplot(dfm, aes(y = .data$id, x = .data$variable, fill = .data$value)) + geom_raster() +
      scale_fill_gradientn(colours = heatC,na.value = 'black') +
      theme(
        panel.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5,vjust = 0.5)
      ) +
      labs(x = "", y = "", title = main)
    
    if (!is.null(YNames)&!all(YNames==1:nrow(Data))) {
      if (length(YNames) != nrow(Data))
        warning(
          "Lengths of YNames does not equal number of rows.\n
          Maybe you want to use:\n
          \t   PixelMatrixPlot(...) + scale_y_reverse(breaks = c(Position of ticks), labels =YNames)"
        )
      plt <-
        plt + scale_y_reverse(breaks = 1:length(YNames), labels = YNames)
    }else{
      plt <- plt + scale_y_reverse()
    }
    if(is.null(XNames)){
      plt=plt+theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    }
    if(is.null(YNames)){
      plt=plt+theme(axis.title.y=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank())
    }
    plt
  }   # end function  PixelMatrixPlot
