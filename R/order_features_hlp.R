order_features_hlp <- function(Data,
                               Ordering = c("Default", "Columnwise", "Alphabetical", "Average", "Variance", "Bimodal", "Statistics"),
                               dvariables = ncol(Data),
                               Ncases = nrow(Data),
                               Npervar = rep(Ncases, ncol(Data)),
                               RobustGaussian=TRUE) {
  
  # Match ordering argument
  Ordering <- match.arg(Ordering)
  
  Effectstrength = rep(0, ncol(Data))
  nonunimodal= Effectstrength
  skewed= Effectstrength
  bimodalprob=Effectstrength
  
  
  
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
             y=c_quantile(x,c(0.25,0.75))
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

return(list(Rangfolge=Rangfolge,Effectstrength=Effectstrength,nonunimodal=nonunimodal,skewed=skewed,bimodalprob=bimodalprob))
}