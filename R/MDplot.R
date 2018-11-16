MDplot = PDEviolinPlot = function(Data, Names,Ordering='Default',Percentalize=FALSE,Fill='darkblue',RobustGaussian=TRUE,GaussianColor='magenta',Gaussian_lwd=1.5,BoxPlot=FALSE,BoxColor='darkred',MDscaling='width',Size=0.01,MinimalAmoutOfData=40){
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
  Ncases=nrow(Data)
  Npervar=apply(Data,MARGIN = 2,function(x) sum(is.finite(x)))
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
  if(Percentalize){
    Data = apply(Data, MARGIN = 2,function(x) return((x-min(x,na.rm = T))/(max(x,na.rm = T) - min(x,na.rm = T)) * 100))
  }
  #First Column is now first variable
  if(any(Npervar<MinimalAmoutOfData)){
    warning(paste('Some columns have less than,',MinimalAmoutOfData,',finite data points. Changing from MD-plot to Jitter-Plot for these columns.'))
    DataDensity=Data
    mm=apply(Data,2,median,na.rm=T)
    DataDensity[,(Npervar<MinimalAmoutOfData)]=mm[(Npervar<MinimalAmoutOfData)]*runif(Ncases, 0.999, 10001)
    DataJitter=Data
    DataJitter[,(Npervar>=MinimalAmoutOfData)]=NaN
    dataframe = reshape2::melt(DataDensity)
  }else{
    dataframe = reshape2::melt(Data)
  }
  colnames(dataframe) <- c('ID', 'Variables', 'Values')
  dataframe$Variables=as.character(dataframe$Variables)
  #Using First Column is first variable principle
  Rangfolge=unique(dataframe$Variables,fromLast = FALSE,nmax = dvariables)#colnames(Data)#
  
  ## Statistics ----
  #Npervar=apply(Data,2,function(x) sum(is.finite(x)))
  #print(Npervar)
  
 if(RobustGaussian==TRUE |Ordering=="Statistics"){
    requireNamespace('moments')
    requireNamespace('diptest')
    x=as.matrix(Data)
    if(dvariables!=ncol(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
    if(Ncases!=nrow(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')

    if(Ncases<50)  warning('Sample of data maybe to small to calculate dip test and argostina test')
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
    bimodalprob=c()
    Nsample=10000
    kernels=matrix(NaN,nrow=Nsample,ncol = dvariables)
    normaldist=matrix(NaN,nrow=Nsample,ncol = dvariables)
    for (i in 1:dvariables) {
      shat[i] <- min(std[i], iqr[i]/faktor, na.rm = TRUE)
      mhat[i] <- mean(x[, i], trim = 0.1, na.rm = TRUE)
      if(Ncases>45000){
        vec=sample(x = x[, i],45000)
        nonunimodal[i]=diptest::dip.test(vec)$p.value
        skewed[i]=moments::agostino.test(vec)$p.value
        bimodalprob[i]=bimodal(vec)$Bimodal
      }
      else if(Npervar[i]<8){
        warning(paste('Sample of finite values to small to calculate agostino.test or dip.test. for row',i,colnames(x)[i]))
        nonunimodal[i]=1
        skewed[i]=1
        bimodalprob[i]=0
      }else{
        nonunimodal[i]=diptest::dip.test(x[, i])$p.value
        skewed[i]=moments::agostino.test(x[, i])$p.value
        bimodalprob[i]=bimodal(x[, i])$Bimodal
      }
      if(nonunimodal[i]>0.05&skewed[i]>0.05&bimodalprob[i]<0.05)
        normaldist[,i] <- rnorm(Nsample, mhat[i], shat[i])
    }
    
    #raw estimation, page 115, projection based clustering book
    nonunimodal[nonunimodal==0]=0.0000000001
    skewed[skewed==0]=0.0000000001
    Effectstrength=(-10*log(skewed)-10*log(nonunimodal))/2
 }#end if (RobustGaussian==TRUE |Ordering=="Statistics"
    
    switch(Ordering,
           Default={
             x=as.matrix(Data)
             if(dvariables!=ncol(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
             if(Ncases!=nrow(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
             
             bimodalprob=c()
             for (i in 1:dvariables) {
               if(Ncases>45000){
                 vec=sample(x = x[, i],45000)
               bimodalprob[i]=bimodal(vec)$Bimodal
               }else if(sum(is.finite(x[, i]))<8){
                 bimodalprob[i]=0
               }else{
                 bimodalprob[i]=bimodal(x[, i])$Bimodal
               }
             }
             #print(bimodalprob)
             Rangfolge=Rangfolge[order(bimodalprob,decreasing = T,na.last = T)]
           },
           Columnwise={Rangfolge=Rangfolge},
           Alphabetical={Rangfolge=sort(Rangfolge,decreasing = F,na.last = T)},
           Statistics={
             Rangfolge=Rangfolge[order(Effectstrength,decreasing = T,na.last = T)]
           },
           {stop('You can select for Ordering: "Default", "Columnwise", "Alphabetical" or "Statistics"')}
           )
  
  ## Plotting ----
  plot =
    ggplot(data = dataframe,
           aes_string(x = "Variables", group = "Variables", y = "Values"))+scale_x_discrete(limits=Rangfolge)
  
  # trim = TRUE: tails of the violins are trimmed
  # Currently catched in PDEdensity anyways but one should be prepared for future ggplot2 changes :-)
  plot=plot + 
    geom_violin(stat = "PDEdensity",fill=Fill,scale=MDscaling,size=Size,trim = TRUE)+ theme(axis.text.x = element_text(size=rel(1.2)))#+coord_flip()
  if(any(Npervar<MinimalAmoutOfData)){
    dataframejitter=reshape2::melt(DataJitter)
    colnames(dataframejitter) <- c('ID', 'Variables', 'Values')
    plot=plot+geom_jitter(size=1.75,data =dataframejitter,aes_string(x = "Variables", group = "Variables", y = "Values"),position=position_jitter(0.15))
    
  }
  if(isTRUE(RobustGaussian)){
    colnames(normaldist)=colnames(Data)
    DFtemp = reshape2::melt(normaldist)
    colnames(DFtemp) <- c('ID', 'Variables', 'Values')
    DFtemp$Variables=as.character(DFtemp$Variables)
    #trimming in this case not required
    plot=plot+geom_violin(data = DFtemp,mapping = aes_string(x = "Variables", group = "Variables", y = "Values"),colour=GaussianColor,alpha=0,scale=MDscaling,size=Gaussian_lwd,na.rm = T,trim = FALSE)+guides(fill=FALSE)
  }
  
  if(isTRUE(BoxPlot)){
    plot=plot+stat_boxplot(geom = "errorbar", width = 0.5, color=BoxColor)+geom_boxplot(width=1,outlier.colour = NA,alpha=0,fill='#ffffff', color=BoxColor)
  }
  # plot=plot + 
  #   geom_violin(stat = "PDEdensity",fill=fill,scale=MDscaling,size=Size)+ theme(axis.text.x = element_text(size=rel(1.2)))
  return(ggplotObj = plot+ggExtra::rotateTextX())
} 