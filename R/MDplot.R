MDplot = function(Data, Names, Ordering='Default',Scaling="None",Fill='darkblue',
                  RobustGaussian=TRUE,GaussianColor='magenta',Gaussian_lwd=1.5,
                  BoxPlot=FALSE,BoxColor='darkred',MDscaling='width',LineColor='black',LineSize=0.01,
                  QuantityThreshold=50, UniqueValuesThreshold=12,SampleSize=5e+05,
                  SizeOfJitteredPoints=1,OnlyPlotOutput=TRUE,main="MD-plot",
                  ylab="Range of values in which PDE is estimated",BW=FALSE,ForceNames=FALSE){
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
  # Author MT 2018: used general idea of FP to apply ggplot2 frameworks 
  
  #always required:
  #requireNamespace("reshape2")
  #if (!requireNamespace('DatabionicSwarm',quietly = TRUE)){
  #  
  #  message('Subordinate package (DatabionicSwarm) is missing. Please install the package which is defined in "Suggests" to select this scaling. Setting scaling to "None".')
  #  
  #  Scaling="None"
 # }
  
  if (!requireNamespace('moments',quietly = TRUE)){
    
    message('Subordinate package (moments) is missing. Please install the package which is defined in "Suggests" to select this Ordering or "RobustGaussian=TRUE". Setting Ordering to "Default" and "RobustGaussian=FALSE".')
    RobustGaussian=FALSE
    Ordering='Default'
  }
  if (!requireNamespace('diptest',quietly = TRUE)){
    
    message('Subordinate package (diptest) is missing. Please install the package which is defined in "Suggests" to select this Ordering or "RobustGaussian=TRUE". Setting Ordering to "Default" and "RobustGaussian=FALSE".')
    RobustGaussian=FALSE
    Ordering='Default'
  }
  
  if (!requireNamespace('signal',quietly = TRUE)){
    
    message('Subordinate package (diptest) is missing. Please install the package which is defined in "Suggests" to select this Ordering or "RobustGaussian=TRUE". Setting Ordering to "Columnwise" and "RobustGaussian=FALSE".')
    RobustGaussian=FALSE
    Ordering='Columnwise'
  }
  if(Ordering=="AsIs") #synonym
    Ordering='Columnwise'
  
  ## Error Catching ----
  if (is.vector(Data)) {
    print("This MD-plot is typically for several features at once. By calling as.matrix(), it will be now used with one feature.")
    Data = as.matrix(Data)
  }
  if(!is.matrix(Data)){
    warning('The MD-plot prefers a numerical matrix. If a data.frame or a tibble is used it is transformed to a matrix. Calling as.matrix.')
    Data=as.matrix(Data) #note to me, in v 1.1.1 DatabionicsSwarm, RobustNormalization only works with matrix, have to fix that..
  }
  if(mode(Data)!='numeric'){
    warning('"mode" of matrix is not numeric. Casting to numeric.')
    mode(Data)='numeric'
  }
  dvariables=ncol(Data)
  
  Ncases=nrow(Data)
  
  Nfinitepervar=apply(Data,MARGIN = 2,function(x) {
    return(sum(is.finite(x)))
  })
  if(any(Nfinitepervar<1)){
    warning('Some columns have not even one finite value. Please check your data. Deleting these columns.')
    Data=Data[,Nfinitepervar>0,drop=FALSE]
    dvariables=ncol(Data)
  }
  
  if(Ncases>SampleSize){
    warning('Data has more cases than "SampleSize". Drawing a sample for faster computation.
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
    
    #addcols=function(...){
    #  return(cbind_fill(...,fill = NaN))
    #}
    nn=colnames(Data)
    Data=do.call(CombineCols,DataListtemp)
    colnames(Data)=nn
    Data=as.matrix(Data)
    #}else{#here alle vectors are sampled
    #   warning('Package rowr is not installed. Sampling Data without taking finite values only into account.')
    #  ind=sample(1:Ncases,size = SampleSize)
    #  Data=Data[ind,,drop=FALSE]
    #}
    Ncases=nrow(Data)
  }
  
  
  Npervar=apply(Data,MARGIN = 2,function(x) sum(is.finite(x)))
  NUniquepervar=apply(Data,MARGIN = 2,function(x) {
    x=x[is.finite(x)]
    return(length(unique(x)))
  })
  
  
  if (missing(Names)) {
    if (!is.null(colnames(Data))) {
      Names = colnames(Data)
      if(length(Names)!=length(unique(Names))){
        warning('Colnames are not unique. Numerating duplicated colnames.')
        # Names = 1:dvariables
        Names=colnames(Data)
        charbooleandupli=duplicated(Names)&Names!=""&Names!=" "
        Names[charbooleandupli]=paste0(Names[charbooleandupli],2:(1+sum(charbooleandupli)))
        colnames(Data) <- Names
      }
      if(any(Names=="")){
        warning('Some colnames are set to "" (blank). Numerating these colnames.')
        Names=colnames(Data)
        charmissing=which(Names=="")
        Names[charmissing]=paste0("C",charmissing)
        colnames(Data) <- Names
      }
      if(any(Names==" ")){
        warning('Some colnames are set to "" (blank). Numerating these colnames.')
        Names=colnames(Data)
        charmissing=which(Names==" ")
        Names[charmissing]=paste0("C",charmissing)
        colnames(Data) <- Names
      }
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
  if(is.null(colnames(Data))){ 
    
    colnames(Data)=paste0('C_',1:dvariables)
    
  }
  #bugfix numeric columns names result in some strange behaviour in either reshape2 or ggplot2
  if(isFALSE(ForceNames)){
    Names=colnames(Data)
    NamesNumeric=suppressWarnings(as.numeric(Names))
    colnames(Data)=ifelse(!is.na(NamesNumeric),paste0('C_',Names),Names)
  }
  # for(i in 1:dvariables){
  #   if(!is.na(NamesNumeric[i])){
  #     colnames(Data)[i]=paste0('C_',NamesNumeric[i])
  #   }
  # }
  
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
  
  
  ## Roboust Gaussian and Statistics ----

  #ToDo: split up RobustGAussian and Ordering Parameter for better code readibility
  #MT: robust gaussians only work with statistical testing, disabled some ifs
  if(RobustGaussian==TRUE |Ordering=="Statistics"){
    if(Ordering=="Statistics"){
      requireNamespace('moments')
      requireNamespace('diptest')
    }
    x=as.matrix(Data)
    #bugfix: statistical testing does not accept infinitive values
    x[x==Inf]=NaN
    x[x==-Inf]=NaN
    if(dvariables!=ncol(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
    if(Ncases!=nrow(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
    
    if(Ncases<50)  warning('Sample of data maybe to small to calculate dip test and argostina test')
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
        quartile[, i] <- quantile(x[, i], probs = p, type = 5, 
                                  na.rm = TRUE)
        MinMax[, i] <- quantile(x[, i], probs = extrema, type = 5, 
                                na.rm = TRUE)
      }
    }else{
      quartile <- quantile(x, p, type = 5, na.rm = TRUE)
      MinMax <- as.matrix(quantile(x, extrema, type = 5, na.rm = TRUE))
    }
    if (dvariables > 1){
      iqr <- quartile[2, ] - quartile[1, ]
    }else{ iqr <- quartile[2] - quartile[1]
    }
    shat <- c()
    mhat <- c()
    nonunimodal=c()
    skewed=c()
    bimodalprob=c()
    isuniformdist=c()
    Nsample=max(c(10000,Ncases))
    #kernels=matrix(NaN,nrow=Nsample,ncol = dvariables)
    normaldist=matrix(NaN,nrow=Nsample,ncol = dvariables)
    for (i in 1:dvariables) {
      shat[i] <- min(std[i], iqr[i]/faktor, na.rm = TRUE)
      mhat[i] <- mean(x[, i], trim = 0.1, na.rm = TRUE)
      if(Ncases>45000&Npervar[i]>8){#statistical testing does not work with to many cases
        vec=sample(x = x[, i],45000)
        # if(Ordering=="Statistics"){
        if(NUniquepervar[i]>UniqueValuesThreshold){
          nonunimodal[i]=diptest::dip.test(vec)$p.value
          skewed[i]=moments::agostino.test(vec)$p.value
          #Ties should not be present, however here we only approximate
          isuniformdist[i]=suppressWarnings(ks.test(vec,"punif", MinMax[1, i], MinMax[2, i])$p.value)
          bimodalprob[i]=bimodal(vec)$Bimodal
        }else{
          warning('Not enough unique values for statistical testing, thus output of testing is ignored.')
          nonunimodal[i]=1
          skewed[i]=1
          isuniformdist[i]=0
          bimodalprob[i]=0
        }
      }else if(Npervar[i]<8){#statistical testing does not work with not enough cases
        warning(paste('Sample of finite values to small to calculate agostino.test or dip.test. for row',i,colnames(x)[i]))
        nonunimodal[i]=1
        skewed[i]=1
        bimodalprob[i]=0
        isuniformdist[i]=0
      }else{
        #        if(Ordering=="Statistics"){
        if(NUniquepervar[i]>UniqueValuesThreshold){
          nonunimodal[i]=diptest::dip.test(x[, i])$p.value
          skewed[i]=moments::agostino.test(x[, i])$p.value
          isuniformdist[i]=suppressWarnings(ks.test(x[, i],"punif", MinMax[1, i], MinMax[2, i])$p.value)
          bimodalprob[i]=bimodal(x[, i])$Bimodal
        }else{#statistical testing requires enough unique values
          warning('Not enough unique values for statistical testing, thus output of testing is ignored.')
          nonunimodal[i]=1
          skewed[i]=1
          isuniformdist[i]=0
          bimodalprob[i]=0
        }
        # }else{
        #   nonunimodal[i]=1
        #   skewed[i]=1
        # }
        
      }
      # if(Ordering=="Statistics"){#everything is siginficant and enough data for gaussian estimation
      if(isuniformdist[i]<0.05 & nonunimodal[i]>0.05&skewed[i]>0.05&bimodalprob[i]<0.05& Npervar[i]>QuantityThreshold & NUniquepervar[i]>UniqueValuesThreshold){
        normaldist[,i] <- rnorm(Nsample, mhat[i], shat[i])
        #trim to range, not exact but close enough
        normaldist[normaldist[,i]<min(x[, i],na.rm=T),i]=NaN#MinMax[1,i]
        normaldist[normaldist[,i]>max(x[, i],na.rm=T),i]=NaN#MinMax[2,i]
      }
    }
    
    #raw estimation, page 115, projection based clustering book
    nonunimodal[nonunimodal==0]=0.0000000001
    skewed[skewed==0]=0.0000000001
    Effectstrength=(-10*log(skewed)-10*log(nonunimodal))/2
    
  }#end if (RobustGaussian==TRUE |Ordering=="Statistics"
  
  ## Ordering ----
  RangfolgeV=order_features_hlp(Data, Ordering,dvariables,Ncases,Npervar,RobustGaussian)  
  Rangfolge=RangfolgeV$Rangfolge
  ## Data Reshaping----
  if(any(Npervar<QuantityThreshold)|any(NUniquepervar<UniqueValuesThreshold)){#builds scatter plots in case of not enough information for pdf
    warning(paste('Some columns have less than,',QuantityThreshold,',finite data points or less than ',UniqueValuesThreshold,' unique values. Changing from MD-plot to Jitter-Plot for these columns.'))
    DataDensity=Data
    #mm=apply(Data,2,median,na.rm=T)
    #this part is obsolete now because geom_jitter does it properly
    #Transforms pdf estimation to median line drawing of pdf cannot be estimated
    #for(nc in 1:dvariables){
    #  if(Npervar[nc]<QuantityThreshold){
    #DataDensity[,nc]=JitterUniqueValues(Data[,nc],NULL)
    
    #generated values around the median if not enoug non finite values given
    # this is done to draw a median line
    # if(mm[nc]!=0){
    #   DataDensity[,nc]=mm[nc]*runif(Ncases, -0.001, 0.001)+mm[nc]
    # }else{
    #   DataDensity[,nc]=runif(Ncases, -0.001, 0.001)
    # }
    #  }
    #  if(NUniquepervar[nc]<UniqueValuesThreshold){
    #DataDensity[,nc]=JitterUniqueValues(Data[,nc],NULL)
    
    #generated values around the median if not enoug unique values given
    # this is done to draw a median line
    # if(mm[nc]!=0){
    #   DataDensity[,nc]=mm[nc]*runif(Ncases, -0.001, 0.001)+mm[nc]
    # }else{
    #   DataDensity[,nc]=runif(Ncases, -0.001, 0.001)
    # }
    #  }
    #}
    #Generates in the cases where pdf cannot be estimated a scatter plot
    DataJitter=DataDensity
    #Delete all scatters for features where distributions can be estimated
    DataJitter[,(Npervar>=QuantityThreshold&NUniquepervar>=UniqueValuesThreshold)]=NaN
    #Delete all distributions for features where jitter plots are provided
    DataDensity[,!(Npervar>=QuantityThreshold&NUniquepervar>=UniqueValuesThreshold)]=NaN
    #apply ordering
    dataframe = reshape2::melt(DataDensity[,Rangfolge])
  }else{
    #apply ordering
    dataframe = reshape2::melt(Data[,Rangfolge])
  }
  
  if(dvariables==1){
    dataframe$ID=rep(1,Ncases)
    dataframe$Variables=rep(colnames(Data),Ncases)
    dataframe$Values=dataframe$value
  }else{
    colnames(dataframe) <- c('ID', 'Variables', 'Values')
  }
  dataframe$Variables=as.character(dataframe$Variables)
  
  
  ## Plotting ----
  
  fillDifferentColors = FALSE
  if(length(Fill) > 1) {
    fillDifferentColors = TRUE
    variableCount = length(unique(dataframe$Variables))
    if(length(Fill) < variableCount) {
      Fill = "darkblue"
      fillDifferentColors = FALSE
      warning('Number of colors is > 1 but does not match the number of variables. Using default coloring')
    } else if(length(Fill) > variableCount) {
      Fill = Fill[1:variableCount]
      warning('Number of colors is greater than number of variables k. Using just the first k colors')
    }
  }
  
  if(fillDifferentColors) {
    plot =
      ggplot(data = dataframe,
             aes(x = .data$Variables, group = .data$Variables, y = .data$Values, fill = .data$Variables))+scale_x_discrete(limits=Rangfolge) 
  } else {
    plot =
      ggplot(data = dataframe,
             aes(x = .data$Variables, group = .data$Variables, y = .data$Values))+scale_x_discrete(limits=Rangfolge) 
  }
  
  if(isTRUE(BW))
    plot=plot+theme_bw()
  # trim = TRUE: tails of the violins are trimmed
  # Currently catched in PDEdensity anyways but one should be prepared for future ggplot2 changes :-)
  if(fillDifferentColors) {
    plot=plot + geom_violin(stat = "PDEdensity", scale = MDscaling, size = LineSize,
                            trim = TRUE, colour = LineColor) + 
      theme(axis.text.x = element_text(size=rel(1.2)), legend.position = "none")
    plot=plot + scale_fill_manual(values=Fill)#+coord_flip()
  } else {
    plot=plot + geom_violin(stat = "PDEdensity", scale = MDscaling, size = LineSize,
                            bounds = c(0, 2),
                            trim = TRUE, fill = Fill, colour = LineColor) +
      theme(axis.text.x = element_text(size=rel(1.2)))
  }
  if(any(Npervar<QuantityThreshold) | any(NUniquepervar<UniqueValuesThreshold)){
    DataJitter[,Rangfolge]
    dataframejitter=reshape2::melt(DataJitter)
    if(dvariables==1){#bugfix one feature
      dataframejitter$ID=rep(1,Ncases)
      dataframejitter$Variables=rep(colnames(DataJitter),Ncases)
      dataframejitter$Values=dataframejitter$value
    }else{
      colnames(dataframejitter) <- c('ID', 'Variables', 'Values')
    }
    if(fillDifferentColors) {
      plot=plot+geom_jitter(size=SizeOfJitteredPoints,data=dataframejitter,aes(x = .data$Variables, group = .data$Variables, y = .data$Values, colour = .data$Variables),
                            height = 0,width=0.15) + scale_color_manual(values = Fill)
    } else {
      plot=plot+geom_jitter(colour=Fill,size=SizeOfJitteredPoints,data=dataframejitter,aes(x = .data$Variables, group = .data$Variables, y = .data$Values),
                            height = 0,width=0.15)#no vertical jitter!
    }
    
    
    
    #geom_jitter(position=position_jitter(0.15))
  }
  
  if(isTRUE(RobustGaussian)){
    colnames(normaldist)=colnames(Data)
    normaldist=normaldist[,Rangfolge]
    if(dvariables==1){#bugfix
      normaldist=as.matrix(normaldist)
      normaldist=cbind(normaldist,rep(NaN,length(normaldist)))
      colnames(normaldist)=c(colnames(Data),'Cnan')
    }
    #bugifix Computation failed in `stat_ydensity()`:replacement has 1 row, data has 0 
    ind_sum=apply(normaldist,2,function(x) sum(!is.finite(x)))
    normaldist=normaldist[,ind_sum!=nrow(normaldist),drop = FALSE]
    
    if(dim(normaldist)[2]>0){
      DFtemp = reshape2::melt(normaldist)
      colnames(DFtemp) <- c('ID', 'Variables', 'Values')
      if(dvariables==1){#bugfix 
        DFtemp=DFtemp[DFtemp[,'Variables']==colnames(Data),]
      }
      DFtemp$Variables=as.character(DFtemp$Variables)
      #trimming in this case not required
      
      
      plot=plot+geom_violin(data = DFtemp,mapping = aes(x = .data$Variables, group = .data$Variables, y = .data$Values),
                            colour=GaussianColor,alpha=0,scale=MDscaling,size=Gaussian_lwd,
                            na.rm = TRUE,trim = TRUE, fill = NA,position="identity",width=1)#+guides(fill=FALSE,scale=MDscaling)
    }#otherwise no robust gaussian exist
  }
  
  if(isTRUE(BoxPlot)){
    plot=plot+stat_boxplot(geom = "errorbar", width = 0.5, color=BoxColor)+geom_boxplot(width=1,outlier.colour = NA,alpha=0,fill='#ffffff', color=BoxColor,position="identity")
  }
  
  if(isTRUE(requireNamespace("ggExtra",quietly = TRUE))){
    plot=plot+ggExtra::rotateTextX()
  }else{
    warning('Package ggExtra is not installed. Labels of Variablenames are not rotated.')
  }
  plot=plot+ggtitle(main)+theme(plot.title = element_text(hjust = 0.5))+ylab(ylab)
  
  if(OnlyPlotOutput){
    return(ggplotObj = plot)
  }else{
    print(plot)
    return(list(Ordering=Rangfolge,DataOrdered=Data[,Rangfolge],ggplotObj = plot))
  }
} 