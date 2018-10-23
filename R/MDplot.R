MDplot = PDEviolinPlot = function(Data, Names,fill='darkblue',StatsEval=FALSE,scale='width',size=0.01){
  #MDplot(data, Names)
  # Plots a Boxplot like pdfshape for each column of the given data
  #
  # Input
  # data          Matrix containing data. Each column is one variable.
  # Names         Optional: Names of the variables. If missing the columnnames of data are used.
  # Means          TRUE: with mean, FALSE: Only median
  #
  # Output
  # ggplotObj     The ggplot object of the boxplots
  #
  # Author MT 2018: rewritten function of FP
  if (is.vector(Data)) {
    warning("This MD-plot is for several features at once. Calling as.matrix()")
    Data = as.matrix(Data)
  }
  if(!is.matrix(Data)){
    warning('The MD-plot prefers a numerical matrix. If a data.frame is used it is transformed to a matrix. Calling as.matrix.')
  Data=as.matrix(Data)
  }
  if(mode(Data)!='numeric'){
    warning('"mode" of matrix is not numeric. Casting to numeric.')
    mode(Data)='numeric'
  }
  if (missing(Names)) {
    if (!is.null(colnames(Data))) {
      Names = colnames(Data)
    } else{
      Names = 1:ncol(Data)
    }
  } else{
    if (length(Names) != ncol(Data)) {
      warning('Length of Names does not equal number of columns of Data. Renaming variables...')
      Names = 1:ncol(Data)
    } else{
      colnames(Data) <- Names
    }
  }

  requireNamespace("reshape2")
  requireNamespace("ggExtra")
  dataframe = reshape2::melt(Data)
  colnames(dataframe) <- c('ID', 'Variables', 'Values')
  dataframe$Variables=as.character(dataframe$Variables)

    plot =
      ggplot(data = dataframe,
             aes_string(x = "Variables", group = "Variables", y = "Values")) +
      geom_violin(stat = "PDEdensity",fill=fill,scale=scale,size=size)+ theme(axis.text.x = element_text(size=rel(1.2)))
    
    if(isTRUE(StatsEval)){
      requireNamespace('moments')
      requireNamespace('diptest')
    x=as.matrix(Data)
    lowInnerPercentile=25
    hiInnerPercentile = 100 - lowInnerPercentile
    faktor <- sum(abs(qnorm(t(c(lowInnerPercentile, hiInnerPercentile)/100), 0, 1)))
    std <- sd(x, na.rm = TRUE)
    p <- c(lowInnerPercentile, hiInnerPercentile)/100
    quartile <- prctile(x, p)
    if (ncol(x) > 1) 
      iqr <- quartile[2, ] - quartile[1, ]
    else iqr <- quartile[2] - quartile[1]
    shat <- c()
    mhat <- c()
    nonunimodal=c()
    skewed=c()
    Nsample=10000
    kernels=matrix(NaN,nrow=Nsample,ncol = ncol(x))
    normaldist=matrix(NaN,nrow=Nsample,ncol = ncol(x))
    for (i in 1:ncol(x)) {
      shat[i] <- min(std[i], iqr[i]/faktor, na.rm = TRUE)
      mhat[i] <- mean(x[, i], trim = 0.1, na.rm = TRUE)
      nonunimodal[i]=diptest::dip.test(x[, i])$p.value
      skewed[i]=moments::agostino.test(x[, i])$p.value
      if(nonunimodal[i]<0.05|skewed[i]<0.05)
        normaldist[,i] <- rnorm(Nsample, mhat[i], shat[i])
    }
    colnames(normaldist)=colnames(Data)
    DFtemp = reshape2::melt(normaldist)
    colnames(DFtemp) <- c('ID', 'Variables', 'Values')
    DFtemp$Variables=as.character(DFtemp$Variables)
    print(shat)
    plot=plot+geom_violin(data = DFtemp,mapping = aes_string(x = "Variables", group = "Variables", y = "Values"),colour='black',alpha=0,scale=scale,size=size)+guides(fill=FALSE)
    #plot=plot+geom_boxplot(width=1,outlier.colour = NA,alpha=0)
    }
  return(ggplotObj = plot+ggExtra::rotateTextX())
} 