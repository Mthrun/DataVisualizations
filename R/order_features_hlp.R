order_features_hlp <- function(Data,
                               Ordering = c("Default", "Columnwise", "Alphabetical", "Average", "Variance", "Bimodal", "Statistics"),
                               iqr,
                               faktor,
                               std,
                               MinMax,
                               dvariables = ncol(Data),
                               Ncases = nrow(Data),
                               Npervar = rep(Ncases, ncol(Data)),
                               NUniquepervar= rep(Ncases, ncol(Data)),
                               UniqueValuesThreshold,
                               RobustGaussian=TRUE) {
  
  # Match ordering argument
  Ordering <- match.arg(Ordering)
  
  Effectstrength = rep(0, ncol(Data))
  nonunimodal= Effectstrength
  skewed= Effectstrength
  bimodalprob=Effectstrength
  isuniformdist=Effectstrength
  
  if(RobustGaussian==TRUE |Ordering=="Statistics"){
    BoolMom=TRUE
      if(!requireNamespace('moments',quietly = T)){
        BoolMom=FALSE
        warning("order_features_hlp: please install package moments")
      }
    BoolDip=TRUE
      if(!requireNamespace('diptest',quietly = T)){
        BoolDip=FALSE
        warning("order_features_hlp: please install package diptest")
      }

    x=as.matrix(Data)
    #bugfix: statistical testing does not accept infinitive values
    x[x==Inf]=NaN
    x[x==-Inf]=NaN
    if(dvariables!=ncol(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
    if(Ncases!=nrow(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
    
    shat <- c()
    mhat <- c()
    # nonunimodal=c()
    # skewed=c()
    # bimodalprob=c()
    # isuniformdist=c()
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
          if(isTRUE(BoolDip))
            nonunimodal[i]=diptest::dip.test(vec)$p.value
          if(isTRUE(BoolMom))
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
          if(isTRUE(BoolDip))
            nonunimodal[i]=diptest::dip.test(x[, i])$p.value
          if(isTRUE(BoolMom))
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
      }
    }#end  for (i in 1:dvariables)
    
    #raw estimation, page 115, projection based clustering book
    nonunimodal[nonunimodal==0]=0.0000000001
    skewed[skewed==0]=0.0000000001
    Effectstrength=(-10*log(skewed)-10*log(nonunimodal))/2
  } # end if(RobustGaussian==TRUE |Ordering=="Statistics")

  
  
dfrang = reshape2::melt(Data)
colnames(dfrang) <- c('ID', 'Variables', 'Values')
dfrang$Variables=as.character(dfrang$Variables)
#Using First Column is first variable principle
Rangfolge=unique(dfrang$Variables,fromLast = FALSE,nmax = dvariables)#colnames(Data)#
rm(dfrang)
switch(Ordering,
       Default={
         x=as.matrix(Data)
         if(dvariables!=ncol(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
         if(Ncases!=nrow(x)) warning('Something went wrong. Dimension of Data changed, but should not. Please contact developer.')
         
         bimodalprob=c()
         for (i in 1:dvariables) {
           if(Ncases>45000 & Npervar[i]>8){
             vec=sample(x = x[, i],45000)
             bimodalprob[i]=bimodal(vec)$Bimodal
           }else if(Npervar[i]<8){
             bimodalprob[i]=0
           }else{
             bimodalprob[i]=bimodal(x[, i])$Bimodal
           }
         }
         
         if(length(unique(bimodalprob))<2 & ncol(x)>1 & isTRUE(RobustGaussian)){
           Rangfolge=Rangfolge[order(Effectstrength,decreasing = T,na.last = T)]
           message('Using statistics for ordering instead of default')
         }else{
           Rangfolge=Rangfolge[order(bimodalprob,decreasing = T,na.last = T)]
         }
       },
       Columnwise={Rangfolge=Rangfolge},
       Alphabetical={Rangfolge=sort(Rangfolge,decreasing = F,na.last = T)},
       Average={
         x=as.matrix(Data)
         med_val=apply(x,2,median,na.rm=TRUE)
         meanval=apply(x,2,function(x) {
           x=x[is.finite(x)]
           if(length(x)>0){
             y=mean(x,trim = 0.1)
             return(y)
           }else{
             return(0)
           }
         })
         average_vals=(med_val+meanval)/2 #weighted average
         average_ind=order(average_vals,na.last = T,decreasing = FALSE)
         Rangfolge=Rangfolge[average_ind]
       },
       Variance={
         x=as.matrix(Data)
         iqr_vals=apply(x,2,function(x) {
           x=x[is.finite(x)]
           if(length(x)>0){
             y=quantile4LargeVectors(x,c(0.25,0.75))
             iqr=y[2]-y[1]
             return(iqr)
           }else{
             return(0)
           }
         })
         iqr_ind=order(iqr_vals,na.last = T,decreasing = FALSE)
         Rangfolge=Rangfolge[iqr_ind]
       },
       Bimodal={
         TMP_X=as.matrix(Data)
         bimodalitycoef=apply(TMP_X,2,function(x) {
           x=x[is.finite(x)]
           if(length(x)>0){
             y=BimodalityAmplitude(x,PlotIt=FALSE)
             if(identical(y, numeric(0))) y=0
             return(y)
           }else{
             return(0)
           }
         }
         )
         bimodal_ind=order(bimodalitycoef,na.last = T,decreasing = TRUE)
         Rangfolge=Rangfolge[bimodal_ind]
       },
       Statistics={
         Rangfolge=Rangfolge[order(Effectstrength,decreasing = T,na.last = T)]
       },
       {stop('You can select for Ordering: "Default", "Columnwise", "Alphabetical", "Average", "Bimodal", "Variance" or "Statistics"')}
)

return(list(Rangfolge=Rangfolge,isuniformdist=isuniformdist,nonunimodal=nonunimodal,skewed=skewed,bimodalprob=bimodalprob))
}