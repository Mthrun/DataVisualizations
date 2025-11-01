MDstrips = function(Data, Ordering='Default',Scaling="None",QuantityThreshold=50, UniqueValuesThreshold=12,SampleSize=5e+05,
                    LabelThreshold = 100,
                    LabelMax = 40,
                    LabelEvery = NULL,
                    LabelVariables = NULL,
                    SizeOfJitteredPoints=1,
                    palette = c("blue","green","yellow","orange","red"),ylab,main,BW=TRUE) {
  
# MDstrips(Data, Ordering='Default',Scaling="None") 
  #
  # MDstrips: Density visualization strips based on Pareto Density Estimation (PDE).
  # Each variable is represented by a vertical strip of colored tiles corresponding
  # to estimated density values. Low-density regions are colored blue/green, 
  # mid-density yellow, and high-density orange/red.
  #
# Input
  # Data                  Matrix containing data. Each column is one variable.
#OPTIONAL
  # Ordering              Optional: string, ordering of variables. Choices are
  #                       "Default", "Columnwise", "AsIs", "Alphabetical",
  #                       "Average", "Bimodal", "Variance", "Statistics".
  # Scaling               Optional: "None", "Percentalize", "CompleteRobust",
  #                       "Robust", or "Log". Defines data scaling before PDE.
  # QuantityThreshold     Minimum number of finite values required to estimate a distribution.
  # UniqueValuesThreshold Minimum number of unique values required to estimate a distribution.
  # SampleSize            Maximum sample size. Larger datasets are subsampled for speed.
  # LabelThreshold        If number of variables > LabelThreshold, not all x-axis labels are shown.
  # LabelMax              Maximum number of x-axis labels to show (if too many variables).
  # LabelEvery            Show every k-th variable label (overrides LabelMax).
  # LabelVariables        Character vector of variables for which labels are shown (overrides others).
  # SizeOfJitteredPoints   Point size used when falling back to a jittered 1D scatter (too few values/unique values).
  # palette               Vector of colors used for density scale (low to high density).
  # ylab                  Y-axis label (range of values in which PDE is estimated).
  # main                  Title of the plot.
  # BW                    Logical. If TRUE, use theme_bw and remove grid lines.
  #
# Output
  # ggplotObj             A ggplot object visualizing the PDE strips.
  #
  # Notes
  # - This method is intended for high-dimensional data visualization with many variables.
  # - PDE is used instead of traditional kernel density estimation for robustness in high dimensions.
  # - Ordering = "Statistics" uses unimodality, skewness, and uniformity tests to arrange variables.
  # - If too few values are present for PDE, jittered points are drawn instead of density strips.
  #
  # Author
  # MT, 10/ 2025. Adapted from MDplot concept, but with strip-based PDE visualization.
  
  if(missing(ylab)){
    ylab="Range of values in which PDE is estimated"
  }
  
  ## Error Catching ----
  if (is.vector(Data)) {
    warning("MDstrips is typically for several features at once. By calling as.matrix(), it will be now used with one feature.")
    Data = as.matrix(Data)
  }
  if(!is.matrix(Data)){
    warning('MDstrips prefers a numerical matrix. If a data.frame or a tibble is used it is transformed to a matrix. Calling as.matrix.')
    Data=as.matrix(Data) #note to me, in v 1.1.1 DatabionicsSwarm, RobustNormalization only works with matrix, have to fix that..
  }
  if(mode(Data)!='numeric'){
    warning('MDstrips: "mode" of matrix is not numeric. Casting to numeric.')
    mode(Data)='numeric'
  }
  if(Ordering=="AsIs") #synonym
    Ordering='Columnwise'
  
  dvariables=ncol(Data)
  
  Ncases=nrow(Data)
  
  Nfinitepervar=apply(Data,MARGIN = 2,function(x) {
    return(sum(is.finite(x)))
  })
  if(any(Nfinitepervar<1)){
    warning('MDstrips: Some columns have not even one finite value. Please check your data. Deleting these columns.')
    Data=Data[,Nfinitepervar>0,drop=FALSE]
    dvariables=ncol(Data)
  }
  
  if(Ncases>SampleSize){
    warning('MDstrips: Data has more cases than "SampleSize". Drawing a sample for faster computation.
    You can omit this by setting "SampleSize=nrow(Data).".')
    #if(isTRUE(requireNamespace('rowr'))){
    #here only finite values are sampled
    indmat=matrix(0,nrow = SampleSize,ncol = dvariables)
    for(i in 1:dvariables){
      ind=which(is.finite(Data[,i]))
      if(length(ind)>=SampleSize){
        indmat[,i]=sample(ind,size = SampleSize)
      }else{
        indmat[1:length(ind),i]=ind
      }
    }
    DataListtemp=mapply(FUN = function(x,y) return(x[y]), as.list(as.data.frame(Data))
                        ,as.list(as.data.frame(indmat)),SIMPLIFY = FALSE)
    
    nn=colnames(Data)
    Data=do.call(CombineCols,DataListtemp)
    colnames(Data)=nn
    Data=as.matrix(Data)

    Ncases=nrow(Data)
  }
  
  if(is.null(colnames(Data))){ 
    
    colnames(Data)=paste0('C_',1:dvariables)
    
  }
  
  Npervar=apply(Data,MARGIN = 2,function(x) sum(is.finite(x)))
  NUniquepervar=apply(Data,MARGIN = 2,function(x) {
    x=x[is.finite(x)]
    return(length(unique(x)))
  })
  
  ## Data Scaling ----
  switch(Scaling,
         None={Data=Data},
         Percentalize={ Data = apply(Data, MARGIN = 2,function(x) return((x-min(x,na.rm = T))/(max(x,na.rm = T) - min(x,na.rm = T)) * 100))},
         CompleteRobust={
           Data=RobustNormalization(Data,Centered = T,Capped = T)},
         Robust={
           Data=RobustNormalization(Data,Centered = F,Capped = F)},
         Log={Data=SignedLog(Data,Base="Ten")
         RobustGaussian=FALSE #log with robust gaussian does not work, because mean and variance is not valid description for log normal data
         },
         {stop('You can select for Scaling: "None", "Percentalize", "CompleteRobust", "Robust" or "Log"')}
  )
  
  ## Data Ordering ----
  if(Ordering=="Statistics"){
    
    x=as.matrix(Data)
    #bugfix: statistical testing does not accept infinitive values
    x[x==Inf]=NaN
    x[x==-Inf]=NaN
    if(dvariables!=ncol(x)) warning('MDstrips: Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
    if(Ncases!=nrow(x)) warning('MDstrips: Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
    
    if(Ncases<50)  warning('MDstrips: Sample of data maybe to small to calculate dip test and argostina test')
    lowInnerPercentile=25
    hiInnerPercentile = 100 - lowInnerPercentile
    faktor <- sum(abs(qnorm(t(c(lowInnerPercentile, hiInnerPercentile)/100), 0, 1)))
    std <- sd(x, na.rm = TRUE)
    p <- c(lowInnerPercentile, hiInnerPercentile)/100
    
    extrema=c(0.001,0.999)
    if (is.matrix(x) && dvariables > 1) {
      cols <- dvariables
      quartile <- matrix(0, nrow = length(p), ncol = cols)
      MinMax <- matrix(0, nrow = length(extrema), ncol = cols)
      for (i in 1:cols) {
        quartile[, i] <- quantile4LargeVectors(x[, i], probs = p)
        MinMax[, i] <- quantile4LargeVectors(x[, i], probs = extrema)
      }
    }else{
      quartile <- quantile4LargeVectors(x, p)
      MinMax <- as.matrix(quantile4LargeVectors(x, extrema))
    }
    if (dvariables > 1){
      iqr <- quartile[2, ] - quartile[1, ]
    }else{
      iqr <- quartile[2] - quartile[1]
    }
  } # end if(RobustGaussian==TRUE |Ordering=="Statistics")
  
  RangfolgeV=order_features_hlp(Data, Ordering,iqr,faktor,std,MinMax,dvariables,Ncases,Npervar,NUniquepervar,UniqueValuesThreshold,RobustGaussian=FALSE)  
  Rangfolge=RangfolgeV$Rangfolge
  
  all_strips = list()
  all_jitter = list()
  ## Choose which x-axis labels to display -----
  d <- length(Rangfolge)
  if (!is.null(LabelVariables)) {
    # show labels only for a user-provided subset (keep order)
    breaks <- Rangfolge[Rangfolge %in% LabelVariables]
    if (length(breaks) == 0) breaks <- NULL
  } else if (!is.null(LabelEvery) && is.finite(LabelEvery) && LabelEvery >= 1) {
    # show every k-th variable
    idx <- seq(1, d, by = LabelEvery)
    breaks <- Rangfolge[idx]
  } else if (d > LabelThreshold) {
    # auto-spacing to ~LabelMax labels
    k <- ceiling(d / max(1, LabelMax))
    idx <- unique(pmin(seq(1, d, by = k), d))
    breaks <- Rangfolge[idx]
  } else {
    # small d: show all labels
    breaks <- Rangfolge
  }

  for (var in Rangfolge) {
    Feature = Data[, var]
    Feature = Feature[is.finite(Feature)]
    # thresholds check
    if (length(Feature) < QuantityThreshold || length(unique(Feature)) < UniqueValuesThreshold) {
      # keep for jitter only
      all_jitter[[var]] <- data.frame(
        Variables = var,
        Values = Feature,
        stringsAsFactors = FALSE
      )
      next   # skip PDE strip for this variable
    }
    
    par=ParetoRadius_fast(Feature)
    densV = ParetoDensityEstimation(Feature,paretoRadius=par,Compute = "Cpp_exp")
    k <- densV$kernels
    # normalize density for color
    d_norm = (densV$paretoDensity - min(densV$paretoDensity)) / (max(densV$paretoDensity) - min(densV$paretoDensity))
    # per-row heights based on local spacing (handles uneven kernels)
    #beware kernels may not be uniformly spaced
    if (length(k) >= 2) {
      dh <- diff(k)
      h_row <- c(dh, tail(dh, 1))  # repeat last spacing for final row
    } else {
      # fallback if only one kernel returned
      rng <- range(Feature, finite = TRUE)
      h_row <- rep(diff(rng), length(k))
    }
    # h_row=(max(k, na.rm = TRUE) - min(k, na.rm = TRUE)) / length(k)
    
    all_strips[[var]] = data.frame(Variable = rep(var, length(k)),
                                    x = k,
                                    PDE = d_norm,
                                   height =as.numeric(h_row),
                                   stringsAsFactors = FALSE)
  }

  df_strips <- if (length(all_strips) > 0) do.call(rbind, all_strips) else NULL
  df_jitter <- if (length(all_jitter) > 0) do.call(rbind, all_jitter) else NULL

  if (!is.null(df_strips)) {
  df_strips$Variable <- factor(df_strips$Variable,
                               levels = Rangfolge,
                               ordered = TRUE)

  obj=ggplot2::ggplot(df_strips, ggplot2::aes_string(x = "Variable", y = "x", fill = "PDE",height="height")) +
    ggplot2::geom_tile(width = 1) +
    ggplot2::scale_fill_gradientn(colors = palette) +
    ggplot2::ylab(ylab) + ggplot2::xlab("Variables")+
    ggplot2::scale_x_discrete(limits = Rangfolge,
                               breaks = breaks,         # show only selected labels
                              expand = c(0, 0)) +     # no gaps between variables
    ggplot2::scale_y_continuous(expand = c(0, 0))   # no outer padding
  }else{
    # fallback empty plot
    obj <- ggplot2::ggplot()
  }
  if (!is.null(df_jitter)) {
    obj <- obj +
      ggplot2::geom_jitter(
        data = df_jitter,
        mapping = ggplot2::aes_string(x = "Variables", y = "Values", group = "Variables"),
        colour = "black", size = SizeOfJitteredPoints,
        height = 0, width = 0.15,
        inherit.aes = FALSE
      )
  }
  
  if(isTRUE(requireNamespace("ggExtra",quietly = TRUE))){
    obj=obj+ggExtra::rotateTextX()
  }else{
    warning('MDstrips: Package ggExtra is not installed. Labels of Variablenames are not rotated.')
  }
  if(!missing(main)){
    obj=obj+ggplot2::ggtitle(main)
  }
    if(isTRUE(BW))
      obj=obj+ggplot2::theme_bw()+ ggplot2::theme(panel.grid.major.x = element_blank(),
                                                  panel.grid.minor.y = element_blank(),
                                                  panel.grid.minor.x = element_blank())
    
  return(obj)
}
