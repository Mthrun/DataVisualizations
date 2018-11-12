MDplot = PDEviolinPlot = function(Data, Names,fill='darkblue',RobustGaussian=TRUE,GaussianColor='magenta',Gaussian_lwd=1.5,BoxPlot=FALSE,BoxColor='darkred',scale='width',size=0.01){
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
  
  ## Error Catching ----
  if (is.vector(Data)) {
    print("This MD-plot is typically for several features at once. By calling as.matrix(), it will be now used with one feature.")
    Data = as.matrix(Data)
  }
  if(!is.matrix(Data)){
    warning('The MD-plot prefers a numerical matrix. If a data.frame or a tibble is used it is transformed to a matrix. Calling as.matrix.')
    Data=as.matrix(Data)
  }
  if(mode(Data)!='numeric'){
    warning('"mode" of matrix is not numeric. Casting to numeric.')
    mode(Data)='numeric'
  }
  dvariables=ncol(Data)
  if (missing(Names)) {
    if (!is.null(colnames(Data))) {
      Names = colnames(Data)
    } else{
      Names = 1:dvariables
    }
  } else{
    if (length(Names) != dvariables) {
      warning('Length of Names does not equal number of columns of Data. Renaming variables...')
      Names = 1:dvariables
    } else{
      colnames(Data) <- Names
    }
  }
  requireNamespace("reshape2")
  requireNamespace("ggExtra")

  ## Data Reshaping ----
  #First Column is now first variable
  dataframe = reshape2::melt(Data)
  colnames(dataframe) <- c('ID', 'Variables', 'Values')
  dataframe$Variables=as.character(dataframe$Variables)
  #Using First Column is first variable principle
  Rangfolge=unique(dataframe$Variables,fromLast = FALSE,nmax = dvariables)#colnames(Data)#
  
  ## Statistics ----
  if(isTRUE(RobustGaussian)){
    requireNamespace('moments')
    requireNamespace('diptest')
    x=as.matrix(Data)
    if(dvariables!=ncol(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
    N=nrow(x)
    if(N<50)  warning('Sample of data maybe to small to calculate dip test and argostina test')
    lowInnerPercentile=25
    hiInnerPercentile = 100 - lowInnerPercentile
    faktor <- sum(abs(qnorm(t(c(lowInnerPercentile, hiInnerPercentile)/100), 0, 1)))
    std <- sd(x, na.rm = TRUE)
    p <- c(lowInnerPercentile, hiInnerPercentile)/100
    if (is.matrix(x) && dvariables > 1) {
      cols <- dvariables
      quartile <- matrix(0, nrow = length(p), ncol = cols)
      for (i in 1:cols) {
        quartile[, i] <- quantile(x[, i], probs = p, type = 5, 
                                na.rm = TRUE)
      }
    }
    else {
      quartile <- quantile(x, p, type = 5, na.rm = TRUE)
    }
    
    if (dvariables > 1) 
      iqr <- quartile[2, ] - quartile[1, ]
    else iqr <- quartile[2] - quartile[1]
    shat <- c()
    mhat <- c()
    nonunimodal=c()
    skewed=c()
    Nsample=10000
    kernels=matrix(NaN,nrow=Nsample,ncol = dvariables)
    normaldist=matrix(NaN,nrow=Nsample,ncol = dvariables)
    for (i in 1:dvariables) {
      shat[i] <- min(std[i], iqr[i]/faktor, na.rm = TRUE)
      mhat[i] <- mean(x[, i], trim = 0.1, na.rm = TRUE)
     
      if(N>45000){
        vec=sample(x = x[, i],45000)
        nonunimodal[i]=diptest::dip.test(vec)$p.value
        skewed[i]=moments::agostino.test(vec)$p.value
      }
      else if(sum(is.finite(x[, i]))<8){
        warning(paste('Sample of finite values to small to calculate agostino.test or dip.test. for row',i,colnames(x)[i]))
        nonunimodal[i]=1
        skewed[i]=1
      }else{
        nonunimodal[i]=diptest::dip.test(x[, i])$p.value
        skewed[i]=moments::agostino.test(x[, i])$p.value
      }
      if(nonunimodal[i]>0.05&skewed[i]>0.05)
        normaldist[,i] <- rnorm(Nsample, mhat[i], shat[i])
    }
    colnames(normaldist)=colnames(Data)
    DFtemp = reshape2::melt(normaldist)
    colnames(DFtemp) <- c('ID', 'Variables', 'Values')
    DFtemp$Variables=as.character(DFtemp$Variables)
    #
    nonunimodal[nonunimodal==0]=0.0000000001
    skewed[skewed==0]=0.0000000001
    Effectstrength=(-log(nonunimodal)-log(skewed))/2
    Rangfolge=Rangfolge[order(Effectstrength,decreasing = T,na.last = T)]
  }
  
  ## Plotting ----
  plot =
    ggplot(data = dataframe,
           aes_string(x = "Variables", group = "Variables", y = "Values"))+scale_x_discrete(limits=Rangfolge)
  
  # trim = TRUE: tails of the violins are trimmed
  # Currently catched in PDEdensity anyways but one should be prepared for future ggplot2 changes :-)
  plot=plot + 
    geom_violin(stat = "PDEdensity",fill=fill,scale=scale,size=size,trim = TRUE)+ theme(axis.text.x = element_text(size=rel(1.2)))#+coord_flip()
  
  if(isTRUE(RobustGaussian)){
    #trimming in this case not required
    plot=plot+geom_violin(data = DFtemp,mapping = aes_string(x = "Variables", group = "Variables", y = "Values"),colour=GaussianColor,alpha=0,scale=scale,size=Gaussian_lwd,na.rm = T,trim = FALSE)+guides(fill=FALSE)
  }
  
  if(isTRUE(BoxPlot)){
    plot=plot+stat_boxplot(geom = "errorbar", width = 0.5, color=BoxColor)+geom_boxplot(width=1,outlier.colour = NA,alpha=0,fill='#ffffff', color=BoxColor)
  }
  # plot=plot + 
  #   geom_violin(stat = "PDEdensity",fill=fill,scale=scale,size=size)+ theme(axis.text.x = element_text(size=rel(1.2)))
  return(ggplotObj = plot+ggExtra::rotateTextX())
} 